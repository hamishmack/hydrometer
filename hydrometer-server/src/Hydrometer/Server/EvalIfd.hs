-- | Evaluation-time builds: import-from-derivation (IFD) and the
-- nix-eval-jobs workers that wait on them.
--
-- hydra-eval-jobset runs nix-eval-jobs with several worker processes.  A
-- worker that needs an IFD blocks until it is built, and another worker that
-- needs the same one blocks behind it, so one slow build can hold every
-- worker (2026-11-03: eight idle for over an hour behind a wasm LLVM), and a
-- failing one is retried by each worker that needs it.  With tracing on,
-- hydra-evaluator's journal has, per evaluation (journald's @_PID@ is the
-- hydra-eval-jobset process, whose command line names the jobset):
--
-- > IFD start pid=500424 n=1 drvs=/nix/store/…-ifd-gen.drv^out at «unknown»
-- > IFD #1: pid=500423 /nix/store/…-ifd-gen.drv^out [built] 9409ms at «unknown»
-- > nej-trace worker-ready pid=500423 init_ms=12 rss_mb=14
-- > nej-trace job pid=500423 ms=9413 ifds=1 ifd_ms=9409 rss_mb=19 kind=job attr=a
-- > nej-trace worker-exit pid=500423 reason=max-memory life_ms=19017 jobs=17 eval_ms=13586 ifd_ms=164 rss_mb=2072
-- > building '/nix/store/…-plan-to-nix-pkgs.drv' on 'ssh://builder@aarch64-darwin-6.lan'...
-- > error: build of '/nix/store/….drv' on 'ssh://builder@aarch64-darwin-9.lan' failed: …
--
-- (@pid=@ in the first two is the worker's.)  Every 30 s the new lines are
-- read and folded into hydrometer.evaluations and its eval_* tables (see
-- sql/005_eval_ifd.sql).
module Hydrometer.Server.EvalIfd
  ( -- * Lines
    Trace (..)
  , parseTrace
  , evaluatorJobset
  , builderName
    -- * Folding them
  , Wait (..)
  , Snap (..)
  , emptySnap
  , WorkerDelta (..)
  , IfdAgg (..)
  , Acc (..)
  , applyTraces
    -- * The scanner
  , runIfdScanner
  , resolveEvaluations
    -- * Queries
  , IfdKey (..)
  , ifdNow
  , evalIfd
  , ifdRuns
  , ifdAlerts
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, displayException, onException, try)
import Control.Monad (forM, forM_, forever, guard, void)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Int (Int64)
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.Read qualified as TR
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray (..))
import System.Directory (doesPathExist)
import System.IO (hClose, hPutStrLn, stderr)
import System.Process

import Hydrometer.Api
import Hydrometer.Server.Db
import Hydrometer.Server.Drv (DrvInfo (..), readDrv)

-- ---------------------------------------------------------------------------
-- Lines

-- | One trace line.  Derivations are .drv paths with any @^outputs@
-- stripped.
data Trace
  = IfdStart !Int ![Text] !Text
    -- ^ A worker (pid) blocks on IFDs of these derivations, at a source
    -- position (or @«unknown»@).  Only logged when the IFD is not instant.
  | IfdDone !(Maybe Int) !Text !Text !Int
    -- ^ A worker's IFD finished: pid (absent in the older format),
    -- derivation, status, milliseconds blocked.  A worker that waited on
    -- another's build reports @already-valid@ with a long wait.
  | WorkerReady !Int !Int !Int
    -- ^ pid, init_ms, rss_mb.
  | WorkerJob !Int !Int !Int !Int !Int !Text !Text
    -- ^ pid, ms, ifds, ifd_ms, rss_mb, kind (job, attrs, error), attr.
  | WorkerExit !Int !Text !Int64 !Int
    -- ^ pid, reason (max-memory, exit), life_ms, rss_mb.
  | Building !Text !(Maybe Text)
    -- ^ A build started, on a remote builder (Nothing: locally).
  | BuildFailed !Text !(Maybe Text) !Text
    -- ^ A build failed: derivation, builder, message.
  deriving stock (Show, Eq)

-- | The lines journalctl selects (matched against MESSAGE).  They are a few
-- in a hundred of hydra-evaluator's (build logs are interleaved), so the
-- journal does the filtering.
journalGrep :: String
journalGrep = "^(IFD (start|#)|nej-trace |building '|error: build of ')"

parseTrace :: Text -> Maybe Trace
parseTrace raw
  | Just r <- T.stripPrefix "IFD start " l = do
      let (kvs, at) = splitAt' r
          kv = pairs kvs
      p <- int =<< lookup "pid" kv
      drvs <- lookup "drvs" kv
      -- Outputs are comma-separated too: @a.drv^out,dev,b.drv^out@.
      pure (IfdStart p (map stripOutputs (filter ("/" `T.isPrefixOf`) (T.splitOn "," drvs))) at)
  | Just r <- T.stripPrefix "IFD #" l = do
      let (body, _) = splitAt' r
      (_n : rest0) <- Just (T.words body)
      let (p, rest) = case rest0 of
            (w : ws) | Just x <- T.stripPrefix "pid=" w -> (int x, ws)
            ws -> (Nothing, ws)
      [drv, st, ms] <- Just rest
      status <- T.stripPrefix "[" st >>= T.stripSuffix "]"
      IfdDone p (stripOutputs drv) status <$> (int =<< T.stripSuffix "ms" ms)
  | Just r <- T.stripPrefix "nej-trace " l = case T.breakOn " " r of
      ("worker-ready", rest) -> let kv = pairs rest in
        WorkerReady <$> get "pid" kv <*> get "init_ms" kv <*> get "rss_mb" kv
      ("job", rest) ->
        -- attr is last and may hold spaces (quoted attribute names).
        let (kvs, attr) = T.breakOn " attr=" rest
            kv = pairs kvs
        in WorkerJob <$> get "pid" kv <*> get "ms" kv <*> get "ifds" kv <*> get "ifd_ms" kv
                     <*> get "rss_mb" kv <*> lookup "kind" kv <*> pure (T.drop 6 attr)
      ("worker-exit", rest) -> let kv = pairs rest in
        WorkerExit <$> get "pid" kv <*> pure (fromMaybe "" (lookup "reason" kv))
                   <*> (fromIntegral <$> (get "life_ms" kv :: Maybe Int)) <*> pure (fromMaybe 0 (get "rss_mb" kv))
      _ -> Nothing
  | Just r <- T.stripPrefix "building '" l = do
      let (drv, after) = T.breakOn "'" r
      guard (isDrv drv)
      case after of
        "'..." -> Just (Building drv Nothing)
        _ -> do
          h <- T.stripPrefix "' on '" after >>= T.stripSuffix "'..."
          pure (Building drv (Just (builderName h)))
  | Just r <- T.stripPrefix "error: build of '" l = do
      let (drv0, after) = T.breakOn "'" r
          drv = stripOutputs drv0
      guard (isDrv drv)
      let (host, rest) = case T.stripPrefix "' on '" after of
            Just a -> let (h, b) = T.breakOn "'" a in (Just (builderName h), T.drop 1 b)
            Nothing -> (Nothing, T.drop 1 after)
          msg = T.strip (T.drop 1 (snd (T.breakOn ":" rest)))
      pure (BuildFailed drv host (T.take 300 msg))
  | otherwise = Nothing
  where
    -- A multi-line message (an IFD line's "stack:" block) is judged by its
    -- first line.
    l = T.takeWhile (/= '\n') raw
    splitAt' t = let (a, b) = T.breakOn " at " t in (a, T.drop 4 b)
    pairs t = [ (k, T.drop 1 v) | w <- T.words t, let (k, v) = T.breakOn "=" w, not (T.null v) ]
    get :: Integral a => Text -> [(Text, Text)] -> Maybe a
    get k kv = int =<< lookup k kv
    isDrv d = "/nix/store/" `T.isPrefixOf` d && ".drv" `T.isSuffixOf` d

int :: Integral a => Text -> Maybe a
int t = case TR.signed TR.decimal t of
  Right (v, "") -> Just v
  _ -> Nothing

-- | @/nix/store/…-foo.drv^out,dev@ -> @/nix/store/…-foo.drv@.
stripOutputs :: Text -> Text
stripOutputs = T.takeWhile (/= '^')

-- | The project and jobset hydra-eval-jobset was started for, from its
-- command line (@… .hydra-eval-jobset-wrapped PROJECT JOBSET@).
evaluatorJobset :: Text -> Maybe (Text, Text)
evaluatorJobset cmd = case dropWhile (not . isEvalJobset) (T.words cmd) of
  (_ : p : j : _) -> Just (p, j)
  _ -> Nothing
  where isEvalJobset w = "hydra-eval-jobset" `T.isInfixOf` T.takeWhileEnd (/= '/') w

-- | A remote store URI as hydra names the machine:
-- @ssh://builder@aarch64-darwin-6.lan@ -> @aarch64-darwin-6@.
builderName :: Text -> Text
builderName u =
  let noScheme = case T.breakOn "://" u of (a, b) | T.null b -> a; (_, b) -> T.drop 3 b
      host = T.takeWhile (\c -> c /= '?' && c /= '/') (T.takeWhileEnd (/= '@') noScheme)
  in fromMaybe host (T.stripSuffix ".lan" host)

-- ---------------------------------------------------------------------------
-- Folding lines into an evaluation's state

-- | A worker blocked on IFDs.
data Wait = Wait
  { waSince :: !UTCTime
  , waDrvs  :: ![Text]
  , waAt    :: !Text
  } deriving stock (Show, Eq)

-- | What an evaluation's workers are doing, as of 'snLast'.
data Snap = Snap
  { snLast    :: !(Maybe UTCTime)
  , snWorkers :: !(Map Int UTCTime)
    -- ^ Workers not known to have exited, and their last line.
  , snWaits   :: !(Map Int Wait)
  } deriving stock (Show, Eq)

emptySnap :: Snap
emptySnap = Snap Nothing M.empty M.empty

-- | One worker's lines in a batch, totalled.
data WorkerDelta = WorkerDelta
  { wdFirst    :: !UTCTime
  , wdLast     :: !UTCTime
  , wdSpawned  :: !(Maybe UTCTime)
  , wdInitMs   :: !(Maybe Int)
  , wdExited   :: !(Maybe UTCTime)
  , wdReason   :: !(Maybe Text)
  , wdLifeMs   :: !(Maybe Int64)
  , wdJobs     :: !Int
  , wdErrors   :: !Int
  , wdEvalMs   :: !Int64
  , wdIfds     :: !Int
  , wdIfdMs    :: !Int64
  , wdRss      :: !(Maybe Int)
  , wdMaxRss   :: !(Maybe Int)
  , wdLastAttr :: !(Maybe Text)
  } deriving stock (Show, Eq)

-- | Finished IFDs of one derivation with one status, in a batch.
data IfdAgg = IfdAgg
  { iaN      :: !Int
  , iaPids   :: !(Set Int)
  , iaWaitMs :: !Int64
  , iaMaxMs  :: !Int
  , iaFirst  :: !UTCTime
  , iaLast   :: !UTCTime
  } deriving stock (Show, Eq)

data Acc = Acc
  { acAllWaitingMs :: !Int64
  , acWorkers      :: !(Map Int WorkerDelta)
  , acIfds         :: !(Map (Text, Text) IfdAgg)
  , acBuilds       :: ![(UTCTime, Text, Maybe Text, Bool, Maybe Text)]
    -- ^ Newest first.
  } deriving stock (Show, Eq)

-- | A worker with no line for this long, and not blocked on an IFD, is
-- taken to be gone (a crashed worker logs no exit line).
quietLimit :: Double
quietLimit = 600

-- | Every active worker is blocked on an IFD.
allWaiting :: UTCTime -> Snap -> Bool
allWaiting t s =
  let active = [ p | (p, lastT) <- M.toList (snWorkers s)
                   , M.member p (snWaits s) || realToFrac (diffUTCTime t lastT) < quietLimit ]
  in not (null active) && all (`M.member` snWaits s) active

-- | Apply an evaluation's lines, in order, to its state.
applyTraces :: Snap -> [(UTCTime, Trace)] -> (Snap, Acc)
applyTraces s0 = foldl' step (s0, Acc 0 M.empty M.empty [])
  where
    step (s, a) (t, tr) =
      let gap = case snLast s of
            Just t0 | t > t0, allWaiting t0 s -> msBetween t0 t
            _ -> 0
          s1 = s { snLast = Just (maybe t (max t) (snLast s)) }
      in apply t tr s1 a { acAllWaitingMs = acAllWaitingMs a + gap }

    apply t tr s a = case tr of
      IfdStart p drvs at ->
        ( s { snWorkers = M.insert p t (snWorkers s), snWaits = M.insert p (Wait t drvs at) (snWaits s) }
        , touch p t id a )
      IfdDone mp drv st ms ->
        ( maybe s (\p -> s { snWorkers = M.insert p t (snWorkers s), snWaits = M.delete p (snWaits s) }) mp
        , maybe id (\p -> touch p t id) mp a
            { acIfds = M.insertWith mergeIfd (drv, st) (IfdAgg 1 (maybe S.empty S.singleton mp) (fromIntegral ms) ms t t) (acIfds a) } )
      WorkerReady p initMs rss ->
        ( s { snWorkers = M.insert p t (snWorkers s) }
        , touch p t (\w -> w { wdSpawned = Just (addUTCTime (negate (fromIntegral initMs / 1000)) t), wdInitMs = Just initMs
                             , wdRss = Just rss, wdMaxRss = max (wdMaxRss w) (Just rss) }) a )
      WorkerJob p ms ifds ifdMs rss kind attr ->
        ( s { snWorkers = M.insert p t (snWorkers s) }
        , touch p t (\w -> w { wdJobs = wdJobs w + 1, wdErrors = wdErrors w + (if kind == "error" then 1 else 0)
                             , wdEvalMs = wdEvalMs w + fromIntegral ms, wdIfds = wdIfds w + ifds
                             , wdIfdMs = wdIfdMs w + fromIntegral ifdMs, wdRss = Just rss
                             , wdMaxRss = max (wdMaxRss w) (Just rss)
                             , wdLastAttr = if T.null attr then wdLastAttr w else Just attr }) a )
      WorkerExit p reason life rss ->
        ( s { snWorkers = M.delete p (snWorkers s), snWaits = M.delete p (snWaits s) }
        , touch p t (\w -> w { wdExited = Just t, wdReason = Just reason, wdLifeMs = Just life
                             , wdRss = Just rss, wdMaxRss = max (wdMaxRss w) (Just rss) }) a )
      Building drv h -> (s, a { acBuilds = (t, drv, h, False, Nothing) : acBuilds a })
      BuildFailed drv h msg -> (s, a { acBuilds = (t, drv, h, True, Just msg) : acBuilds a })

    touch p t f a = a { acWorkers = M.alter (Just . f . bump t . fromMaybe (newDelta t)) p (acWorkers a) }
    bump t w = w { wdLast = max t (wdLast w) }
    newDelta t = WorkerDelta t t Nothing Nothing Nothing Nothing Nothing 0 0 0 0 0 Nothing Nothing Nothing
    mergeIfd new old = IfdAgg
      { iaN = iaN old + iaN new, iaPids = S.union (iaPids old) (iaPids new)
      , iaWaitMs = iaWaitMs old + iaWaitMs new, iaMaxMs = max (iaMaxMs old) (iaMaxMs new)
      , iaFirst = min (iaFirst old) (iaFirst new), iaLast = max (iaLast old) (iaLast new) }

msBetween :: UTCTime -> UTCTime -> Int64
msBetween a b = round (realToFrac (diffUTCTime b a) * 1000 :: Double)

-- ---------------------------------------------------------------------------
-- The scanner

scannerName :: Text
scannerName = "hydra-evaluator-ifd"

-- | One journal entry: cursor, time, _PID, the jobset from _CMDLINE, and the
-- line if it is a trace line.
data Entry = Entry
  { enCursor :: !Text
  , enTime   :: !UTCTime
  , enPid    :: !(Maybe Int)
  , enJobset :: !(Maybe (Text, Text))
  , enTrace  :: !(Maybe Trace)
  }

entry :: BL.ByteString -> Maybe Entry
entry l = do
  A.Object o <- A.decode l
  cur <- str =<< KM.lookup "__CURSOR" o
  us <- str =<< KM.lookup "__REALTIME_TIMESTAMP" o
  t <- posixSecondsToUTCTime . (/ 1000000) . fromIntegral <$> (int us :: Maybe Integer)
  pure Entry
    { enCursor = cur, enTime = t
    , enPid = int =<< str =<< KM.lookup "_PID" o
    , enJobset = evaluatorJobset =<< str =<< KM.lookup "_CMDLINE" o
    , enTrace = parseTrace =<< message =<< KM.lookup "MESSAGE" o }
  where
    str = \case A.String s -> Just s; _ -> Nothing
    -- journald gives a non-UTF-8 message as an array of bytes.
    message = \case
      A.String s -> Just s
      A.Array bs -> Just (T.decodeUtf8With T.lenientDecode (BS.pack [ fromIntegral (truncate n :: Int) | A.Number n <- foldr (:) [] bs ]))
      _ -> Nothing

-- | hydra-eval-jobset processes seen recently: pid -> (evaluation, jobset,
-- last line).  Lines logged after the process exited have no _CMDLINE, so
-- they are attributed by pid.
type PidCache = IORef (Map Int (Int64, (Text, Text), UTCTime))

runIfdScanner :: Db -> IO ()
runIfdScanner d = do
  cache <- newIORef M.empty
  forever $ do
    r <- try (scanJournal d cache) :: IO (Either SomeException ())
    case r of
      Right () -> pure ()
      Left e -> do
        -- A rolled-back chunk may have cached evaluations that are not in
        -- the database.
        writeIORef cache M.empty
        hPutStrLn stderr ("IFD scanner: " <> displayException e)
    threadDelay (30 * 1000000)

-- | Read the journal from the saved cursor (the last hour the first time),
-- a chunk at a time, each chunk and the cursor after it in one transaction.
scanJournal :: Db -> PidCache -> IO ()
scanJournal d cache = do
  saved <- withConn d $ \c -> query c "SELECT cursor FROM hydrometer.journal_cursors WHERE scanner = ?" (Only scannerName)
  let from = case saved of
        [Only cur] -> ["--after-cursor=" <> T.unpack cur]
        _ -> ["--since=-1h"]
  (_, Just hout, Just herr, ph) <- createProcess (proc "journalctl" (args ++ from))
    { std_out = CreatePipe, std_err = CreatePipe }
  errs <- async (BS.hGetContents herr)
  let run = do
        out <- BL.hGetContents hout
        forM_ (chunks (mapMaybe entry (BLC.lines out))) $ \ch -> withTx d $ \c -> do
          processChunk c cache ch
          void $ execute c
            "INSERT INTO hydrometer.journal_cursors (scanner, cursor) VALUES (?, ?) \
            \ON CONFLICT (scanner) DO UPDATE SET cursor = EXCLUDED.cursor, t = now()"
            (scannerName, enCursor (last ch))
  run `onException` terminateProcess ph
  _ <- wait errs
  hClose hout
  void (waitForProcess ph)
  where
    args = [ "--no-pager", "--unit=hydra-evaluator", "--output=json"
           , "--output-fields=MESSAGE,_PID,_CMDLINE", "--grep=" <> journalGrep ]
    chunks [] = []
    chunks xs = let (a, b) = splitAt 5000 xs in a : chunks b

processChunk :: Connection -> PidCache -> [Entry] -> IO ()
processChunk c cache es = do
  tagged <- fmap catMaybes $ forM [ (e, tr) | e <- es, Just tr <- [enTrace e] ] $ \(e, tr) -> case enPid e of
    Nothing -> pure Nothing
    Just p -> fmap (\ev -> (ev, [(enTime e, tr)])) <$> evaluationFor c cache (enTime e) p (enJobset e)
  -- Newest first per evaluation, so reversed.
  forM_ (M.toList (M.fromListWith (++) tagged)) $ \(ev, rev) -> do
    s <- loadSnap c ev
    let (s', a) = applyTraces s (reverse rev)
    writeEval c ev s' a

-- | The evaluation a line belongs to, creating it at its first line.
evaluationFor :: Connection -> PidCache -> UTCTime -> Int -> Maybe (Text, Text) -> IO (Maybe Int64)
evaluationFor c cache t p mjs = do
  m <- readIORef cache
  let fresh (_, _, lastT) = diffUTCTime t lastT < 6 * 3600
      remember i js = modifyIORef' cache (M.insert p (i, js, t)) >> pure (Just i)
  case (M.lookup p m, mjs) of
    (Just x@(i, js', _), Just js) | js' == js, fresh x -> remember i js
    (Just x@(i, js', _), Nothing) | fresh x -> remember i js'
    (_, Just js@(proj, jobset)) -> do
      found <- query c
        "SELECT id FROM hydrometer.evaluations WHERE pid = ? AND project = ? AND jobset = ? AND last_seen > ? \
        \ORDER BY id DESC LIMIT 1" (p, proj, jobset, addUTCTime (-6 * 3600) t)
      i <- case found of
        (Only i : _) -> pure i
        [] -> do
          [Only i] <- query c
            "INSERT INTO hydrometer.evaluations (pid, project, jobset, first_seen, last_seen) \
            \VALUES (?, ?, ?, ?, ?) RETURNING id" (p, proj, jobset, t, t)
          pure i
      remember i js
    (_, Nothing) -> do
      found <- query c
        "SELECT id, project, jobset FROM hydrometer.evaluations WHERE pid = ? AND last_seen > ? \
        \ORDER BY id DESC LIMIT 1" (p, addUTCTime (-6 * 3600) t)
      case found of
        ((i, proj, jobset) : _) -> remember i (proj, jobset)
        [] -> pure Nothing

loadSnap :: Connection -> Int64 -> IO Snap
loadSnap c ev = do
  lastT <- query c "SELECT last_seen FROM hydrometer.evaluations WHERE id = ?" (Only ev)
  ws <- query c "SELECT pid, last_seen FROM hydrometer.eval_workers WHERE evaluation = ? AND exited IS NULL" (Only ev)
  wts <- query c "SELECT pid, since, drvs, coalesce(at, '') FROM hydrometer.eval_ifd_waits WHERE evaluation = ?" (Only ev)
  pure Snap
    { snLast = case lastT of (Only x : _) -> Just x; [] -> Nothing
    , snWorkers = M.fromList ws
    , snWaits = M.fromList [ (p, Wait since (fromPGArray drvs) at) | (p, since, drvs, at) <- wts ] }

writeEval :: Connection -> Int64 -> Snap -> Acc -> IO ()
writeEval c ev s a = do
  void $ execute c
    "UPDATE hydrometer.evaluations SET last_seen = greatest(last_seen, ?), all_waiting_ms = all_waiting_ms + ? WHERE id = ?"
    (snLast s, acAllWaitingMs a, ev)
  void $ execute c "DELETE FROM hydrometer.eval_ifd_waits WHERE evaluation = ?" (Only ev)
  void $ executeMany c "INSERT INTO hydrometer.eval_ifd_waits (evaluation, pid, since, drvs, at) VALUES (?, ?, ?, ?, ?)"
    [ (ev, p, waSince w, PGArray (waDrvs w), waAt w) | (p, w) <- M.toList (snWaits s) ]
  void $ executeMany c
    "INSERT INTO hydrometer.eval_workers AS w (evaluation, pid, spawned, init_ms, first_seen, last_seen, exited, \
    \  exit_reason, life_ms, jobs, job_errors, eval_ms, ifds, ifd_ms, rss_mb, max_rss_mb, last_attr) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (evaluation, pid) DO UPDATE SET \
    \  spawned = coalesce(EXCLUDED.spawned, w.spawned), init_ms = coalesce(EXCLUDED.init_ms, w.init_ms), \
    \  last_seen = greatest(w.last_seen, EXCLUDED.last_seen), exited = coalesce(EXCLUDED.exited, w.exited), \
    \  exit_reason = coalesce(EXCLUDED.exit_reason, w.exit_reason), life_ms = coalesce(EXCLUDED.life_ms, w.life_ms), \
    \  jobs = w.jobs + EXCLUDED.jobs, job_errors = w.job_errors + EXCLUDED.job_errors, \
    \  eval_ms = w.eval_ms + EXCLUDED.eval_ms, ifds = w.ifds + EXCLUDED.ifds, ifd_ms = w.ifd_ms + EXCLUDED.ifd_ms, \
    \  rss_mb = coalesce(EXCLUDED.rss_mb, w.rss_mb), max_rss_mb = greatest(w.max_rss_mb, EXCLUDED.max_rss_mb), \
    \  last_attr = coalesce(EXCLUDED.last_attr, w.last_attr)"
    [ (ev, p, wdSpawned w, wdInitMs w, wdFirst w, wdLast w, wdExited w, wdReason w, wdLifeMs w, wdJobs w)
      :. (wdErrors w, wdEvalMs w, wdIfds w, wdIfdMs w, wdRss w, wdMaxRss w, wdLastAttr w)
    | (p, w) <- M.toList (acWorkers a) ]
  void $ executeMany c
    "INSERT INTO hydrometer.eval_ifds AS i (evaluation, drv, status, n, pids, wait_ms, max_ms, first_done, last_done) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (evaluation, drv, status) DO UPDATE SET n = i.n + EXCLUDED.n, \
    \  pids = ARRAY(SELECT DISTINCT x FROM unnest(i.pids || EXCLUDED.pids) x ORDER BY x), \
    \  wait_ms = i.wait_ms + EXCLUDED.wait_ms, max_ms = greatest(i.max_ms, EXCLUDED.max_ms), \
    \  first_done = least(i.first_done, EXCLUDED.first_done), last_done = greatest(i.last_done, EXCLUDED.last_done)"
    [ (ev, drv, st, iaN x, PGArray (S.toList (iaPids x)), iaWaitMs x, iaMaxMs x, iaFirst x, iaLast x)
    | ((drv, st), x) <- M.toList (acIfds a) ]
  void $ executeMany c "INSERT INTO hydrometer.eval_builds (evaluation, t, drv, host, failed, error) VALUES (?, ?, ?, ?, ?, ?)"
    [ (ev, t, drv, h, failed, err) | (t, drv, h, failed, err) <- reverse (acBuilds a) ]

-- | Match evaluations to hydra's jobsetevals rows.  Hydra writes the row
-- when the evaluation finishes (timestamp = then), and none at all when it
-- fails or finds nothing new, so take the row of the jobset written after
-- this evaluation started and before the jobset's next one did.
resolveEvaluations :: Connection -> IO Int64
resolveEvaluations c = execute_ c
  "WITH cand AS ( \
  \  SELECT DISTINCT ON (v.id) v.id, j.id AS jobset_id, e.id AS eval_id \
  \  FROM hydrometer.evaluations v \
  \  JOIN jobsets j ON j.project = v.project AND j.name = v.jobset \
  \  JOIN jobsetevals e ON e.jobset_id = j.id \
  \    AND e.timestamp >= extract(epoch FROM v.first_seen) - 300 \
  \    AND e.timestamp <= extract(epoch FROM coalesce( \
  \      (SELECT min(w.first_seen) FROM hydrometer.evaluations w \
  \       WHERE w.project = v.project AND w.jobset = v.jobset AND w.first_seen > v.first_seen), \
  \      v.last_seen + interval '1 hour')) \
  \  WHERE v.eval_id IS NULL AND v.last_seen < now() - interval '2 minutes' \
  \    AND v.last_seen > now() - interval '2 days' \
  \  ORDER BY v.id, abs(e.timestamp - extract(epoch FROM v.last_seen)) \
  \) \
  \UPDATE hydrometer.evaluations v SET jobset_id = cand.jobset_id, eval_id = cand.eval_id \
  \FROM cand WHERE cand.id = v.id"

-- ---------------------------------------------------------------------------
-- Queries

data IfdKey
  = IfdRun Int64
  | IfdEval Int
    -- ^ hydra's jobsetevals.id.
  | IfdLatest (Maybe Text) Text
    -- ^ The latest evaluation of a jobset (in any project when Nothing).

-- | The IFD state of the running evaluation of @project:jobset@ that
-- started at @startedMs@, if it has logged any.
ifdNow :: Connection -> Text -> Text -> Int64 -> IO (Maybe IfdNow)
ifdNow c proj jobset startedMs = do
  rs <- query c
    "SELECT id, all_waiting_ms FROM hydrometer.evaluations \
    \WHERE project = ? AND jobset = ? AND first_seen >= to_timestamp(?::bigint / 1000.0) - interval '2 minutes' \
    \ORDER BY id DESC LIMIT 1" (proj, jobset, startedMs)
  case rs of
    ((run, allW) : _) -> Just <$> liveIfd c run allW
    [] -> pure Nothing

liveIfd :: Connection -> Int64 -> Int64 -> IO IfdNow
liveIfd c run allW = do
  waits <- query c
    "SELECT pid, (extract(epoch FROM since) * 1000)::bigint, coalesce(drvs[1], '') FROM hydrometer.eval_ifd_waits \
    \WHERE evaluation = ?" (Only run) :: IO [(Int, Int64, Text)]
  workers <- query c
    "SELECT w.pid, (extract(epoch FROM w.spawned) * 1000)::bigint, w.jobs, (extract(epoch FROM w.last_seen) * 1000)::bigint, w.rss_mb \
    \FROM hydrometer.eval_workers w WHERE w.evaluation = ? AND w.exited IS NULL \
    \  AND (w.last_seen > now() - interval '10 minutes' \
    \    OR EXISTS (SELECT 1 FROM hydrometer.eval_ifd_waits x WHERE x.evaluation = w.evaluation AND x.pid = w.pid)) \
    \ORDER BY w.pid" (Only run) :: IO [(Int, Maybe Int64, Int, Int64, Maybe Int)]
  let waitDrvs = S.toList (S.fromList [ d | (_, _, d) <- waits ])
  hosts <- if null waitDrvs then pure [] else query c
    "SELECT DISTINCT ON (drv) drv, host, (extract(epoch FROM t) * 1000)::bigint FROM hydrometer.eval_builds \
    \WHERE evaluation = ? AND NOT failed AND drv = ANY (?) ORDER BY drv, t DESC" (run, PGArray waitDrvs)
    :: IO [(Text, Maybe Text, Int64)]
  recent <- query c
    "SELECT drv, host, ms FROM ( \
    \  SELECT DISTINCT ON (drv) drv, host, failed, (extract(epoch FROM t) * 1000)::bigint AS ms \
    \  FROM hydrometer.eval_builds WHERE evaluation = ? AND t > now() - interval '6 hours' ORDER BY drv, t DESC \
    \) b WHERE NOT failed ORDER BY ms DESC LIMIT 20" (Only run) :: IO [(Text, Maybe Text, Int64)]
  -- A build is over once its outputs are in the store (remote builds are
  -- copied back).  Content-addressed outputs have no path to look for.
  building <- fmap catMaybes $ forM recent $ \(drv, h, since) -> do
    info <- readDrv drv
    done <- case dOutputs <$> info of
      Just outs@(_ : _) -> and <$> mapM (doesPathExist . T.unpack) outs
      _ -> pure False
    pure (if done then Nothing else Just (IfdBuilding drv h since))
  [(ifds, waitMs)] <- query c
    "SELECT coalesce(sum(n), 0)::int, coalesce(sum(wait_ms), 0)::bigint FROM hydrometer.eval_ifds WHERE evaluation = ?" (Only run)
  let waitOf = M.fromList [ (p, (since, d)) | (p, since, d) <- waits ]
      hostOf = M.fromList [ (d, (h, t)) | (d, h, t) <- hosts ]
      grouped = M.fromListWith (\(n1, s1) (n2, s2) -> (n1 + n2, min s1 s2)) [ (d, (1 :: Int, since)) | (_, since, d) <- waits ]
  pure IfdNow
    { inRun = fromIntegral run
    , inWorkers =
        [ IfdWorker { wkPid = p, wkSince = sp, wkJobs = jobs, wkLastActive = lastT, wkRssMb = rss
                    , wkWaitingOn = snd <$> M.lookup p waitOf, wkWaitingSince = fst <$> M.lookup p waitOf }
        | (p, sp, jobs, lastT, rss) <- workers ]
    , inWaits = sortOn (\w -> (Down (wtWorkers w), wtSince w))
        [ IfdWait { wtDrv = d, wtWorkers = n, wtSince = since
                  , wtHost = fst =<< M.lookup d hostOf, wtBuildingSince = snd <$> M.lookup d hostOf }
        | (d, (n, since)) <- M.toList grouped ]
    , inBuilding = take 8 building
    , inIfds = ifds
    , inWaitMs = waitMs
    , inAllWaitingMs = allW }

-- | One evaluation's IFDs and workers.
evalIfd :: Connection -> IfdKey -> IO (Maybe EvalIfd)
evalIfd c key = do
  rows <- case key of
    IfdRun r -> query c (runSql "v.id = ?" "") (Only r)
    IfdEval e -> query c (runSql "v.eval_id = ?" "ORDER BY v.last_seen DESC") (Only e)
    IfdLatest (Just p) j -> query c (runSql "v.project = ? AND v.jobset = ?" "ORDER BY v.id DESC") (p, j)
    IfdLatest Nothing j -> query c (runSql "v.jobset = ?" "ORDER BY v.id DESC") (Only j)
  case rows of
    [] -> pure Nothing
    (((run, pid, proj, jobset, evalId) :. (started, lastSeen, allW, running)) : _) -> do
      byStatus <- query c
        "SELECT status, sum(n)::int, sum(wait_ms)::bigint FROM hydrometer.eval_ifds WHERE evaluation = ? GROUP BY 1"
        (Only run) :: IO [(Text, Int, Int64)]
      inFlight <- if running then query c
          "SELECT coalesce(sum(extract(epoch FROM now() - since) * 1000), 0)::bigint \
          \FROM hydrometer.eval_ifd_waits WHERE evaluation = ?" (Only run)
        else pure [Only (0 :: Int64)]
      [(workers, jobs, jobErrors, maxRss, medLife, medJobs, meanInit, workerMs)] <- query c
        "SELECT count(*)::int, coalesce(sum(jobs), 0)::int, coalesce(sum(job_errors), 0)::int, max(max_rss_mb), \
        \  (percentile_cont(0.5) WITHIN GROUP (ORDER BY life))::bigint, \
        \  percentile_cont(0.5) WITHIN GROUP (ORDER BY jobs), avg(init_ms)::float8, coalesce(sum(life), 0)::bigint \
        \FROM (SELECT w.jobs, w.job_errors, w.max_rss_mb, w.init_ms, \
        \  coalesce(w.life_ms, (extract(epoch FROM greatest(w.last_seen, \
        \    CASE WHEN ? AND EXISTS (SELECT 1 FROM hydrometer.eval_ifd_waits x WHERE x.evaluation = w.evaluation AND x.pid = w.pid) \
        \    THEN now() END) - coalesce(w.spawned, w.first_seen)) * 1000)::bigint) AS life \
        \  FROM hydrometer.eval_workers w WHERE w.evaluation = ?) w"
        (running, run)
      exits <- query c
        "SELECT coalesce(exit_reason, 'none'), count(*)::int FROM hydrometer.eval_workers WHERE evaluation = ? GROUP BY 1"
        (Only run) :: IO [(Text, Int)]
      [(builds, buildFailures)] <- query c
        "SELECT count(*) FILTER (WHERE NOT failed)::int, count(*) FILTER (WHERE failed)::int \
        \FROM hydrometer.eval_builds WHERE evaluation = ?" (Only run)
      top <- queryTable c
        "SELECT hydrometer.drv_name(i.drv) AS drv_name, string_agg(i.status, ', ' ORDER BY i.wait_ms DESC) AS status, \
        \  sum(i.n)::int AS ifds, \
        \  (SELECT count(DISTINCT p)::int FROM hydrometer.eval_ifds x, unnest(x.pids) p \
        \   WHERE x.evaluation = i.evaluation AND x.drv = i.drv) AS workers, \
        \  round(sum(i.wait_ms) / 1000.0, 1) AS wait_s, round(max(i.max_ms) / 1000.0, 1) AS max_wait_s, \
        \  (extract(epoch FROM max(i.last_done)) * 1000)::bigint AS last_done, i.drv \
        \FROM hydrometer.eval_ifds i WHERE i.evaluation = ? \
        \GROUP BY i.evaluation, i.drv ORDER BY sum(i.wait_ms) DESC LIMIT 25" (Only run)
      failed <- queryTable c
        ("SELECT hydrometer.drv_name(drv) AS drv_name, status, n AS attempts, cardinality(pids) AS workers, \
        \  round(wait_ms / 1000.0, 1) AS wait_s, (extract(epoch FROM first_done) * 1000)::bigint AS first_done, \
        \  (extract(epoch FROM last_done) * 1000)::bigint AS last_done, drv \
        \FROM hydrometer.eval_ifds WHERE evaluation = ? AND status NOT IN " <> okStatuses <> " \
        \ORDER BY n DESC, wait_ms DESC LIMIT 50") (Only run)
      buildErrors <- queryTable c
        "SELECT hydrometer.drv_name(drv) AS drv_name, host AS machine, count(*)::int AS failures, \
        \  (extract(epoch FROM max(t)) * 1000)::bigint AS last_done, (array_agg(error ORDER BY t DESC))[1] AS error, drv \
        \FROM hydrometer.eval_builds WHERE evaluation = ? AND failed \
        \GROUP BY drv, host ORDER BY failures DESC, max(t) DESC LIMIT 30" (Only run)
      live <- if running then Just <$> liveIfd c run allW else pure Nothing
      let flight = case inFlight of (Only x : _) -> x; [] -> 0
      pure $ Just EvalIfd
        { eiRun = fromIntegral run, eiPid = pid, eiProject = proj, eiJobset = jobset, eiEvalId = evalId
        , eiStarted = started, eiLastSeen = lastSeen, eiRunning = running
        , eiIfds = sum [ n | (_, n, _) <- byStatus ]
        , eiByStatus = M.fromList [ (st, n) | (st, n, _) <- byStatus ]
        , eiWaitMs = sum [ w | (_, _, w) <- byStatus ] + flight
        , eiWorkerMs = workerMs, eiAllWaitingMs = allW
        , eiWorkers = workers, eiExitReasons = M.fromList exits
        , eiMedianLifeMs = medLife, eiMedianJobs = medJobs, eiMeanInitMs = meanInit
        , eiJobs = jobs, eiJobErrors = jobErrors, eiMaxRssMb = maxRss
        , eiBuilds = builds, eiBuildFailures = buildFailures
        , eiTop = top, eiFailed = failed, eiBuildErrors = buildErrors, eiNow = live }
  where
    runSql :: Query -> Query -> Query
    runSql cond order =
      "SELECT v.id, v.pid, v.project, v.jobset, v.eval_id, \
      \  (extract(epoch FROM v.first_seen) * 1000)::bigint, \
      \  (extract(epoch FROM CASE WHEN r.running THEN greatest(v.last_seen, now()) ELSE v.last_seen END) * 1000)::bigint, \
      \  v.all_waiting_ms, r.running \
      \FROM hydrometer.evaluations v, LATERAL (SELECT " <> runningSql <> " AS running) r \
      \WHERE " <> cond <> " " <> order <> " LIMIT 1"

-- | Statuses of an IFD that did not fail.
okStatuses :: Query
okStatuses = "('built', 'substituted', 'already-valid', 'resolves-to-already-valid')"

-- | Whether evaluations row @v@ is still running: hydra says its jobset is
-- being evaluated, by a process that started no earlier than this one.
runningSql :: Query
runningSql =
  "(v.eval_id IS NULL \
  \  AND EXISTS (SELECT 1 FROM jobsets j WHERE j.project = v.project AND j.name = v.jobset \
  \    AND j.starttime IS NOT NULL AND v.first_seen >= to_timestamp(j.starttime) - interval '2 minutes') \
  \  AND NOT EXISTS (SELECT 1 FROM hydrometer.evaluations w \
  \    WHERE w.project = v.project AND w.jobset = v.jobset AND w.id > v.id))"

-- | Recent evaluations with their IFD totals, newest first.
ifdRuns :: Connection -> Int -> IO Table
ifdRuns c limit = queryTable c
  ("SELECT v.id AS run, v.project, v.jobset, v.eval_id, (extract(epoch FROM v.first_seen) * 1000)::bigint AS started, \
  \  extract(epoch FROM v.last_seen - v.first_seen)::int AS span_s, " <> runningSql <> " AS running, \
  \  i.ifds, i.ifd_wait_s, i.failed_ifds, round(v.all_waiting_ms / 1000.0) AS all_waiting_s, \
  \  (SELECT count(*)::int FROM hydrometer.eval_workers w WHERE w.evaluation = v.id) AS workers \
  \FROM hydrometer.evaluations v \
  \LEFT JOIN LATERAL (SELECT sum(n)::int AS ifds, round(sum(wait_ms) / 1000.0) AS ifd_wait_s, \
  \    (sum(n) FILTER (WHERE status NOT IN " <> okStatuses <> "))::int AS failed_ifds \
  \  FROM hydrometer.eval_ifds WHERE evaluation = v.id) i ON true \
  \ORDER BY v.id DESC LIMIT ?") (Only limit)

-- | A convoy: at least half of a running evaluation's live workers (and
-- two or more) blocked on the same IFD for five minutes or more.
ifdAlerts :: Int64 -> [EvalNow] -> [Alert]
ifdAlerts now evals =
  [ Alert (if waited >= 30 * 60000 then "critical" else "warning") $
      showT (wtWorkers w) <> " of " <> showT live <> " evaluation workers of " <> eProject e <> ":" <> eJobset e
      <> " have waited " <> dur waited <> " on the import-from-derivation build of " <> drvName (wtDrv w)
      <> maybe "" (\h -> ", building on " <> h <> maybe "" (\t -> " for " <> dur (now - t)) (wtBuildingSince w)) (wtHost w)
      <> ". A worker blocked on it evaluates nothing else."
  | e <- evals, Just i <- [eIfd e]
  , let live = length (inWorkers i)
  , w : _ <- [inWaits i]
  , wtWorkers w >= 2, wtWorkers w * 2 >= live
  , let waited = now - wtSince w
  , waited >= 5 * 60000 ]
  where
    showT :: Show a => a -> Text
    showT = T.pack . show
    dur ms = let m = ms `div` 60000 in
      if m >= 60 then showT (m `div` 60) <> "h" <> T.justifyRight 2 '0' (showT (m `mod` 60)) <> "m" else showT m <> "m"
    drvName p =
      let base = T.takeWhileEnd (/= '/') p
          noHash = if T.length base > 33 && T.index base 32 == '-' then T.drop 33 base else base
      in fromMaybe noHash (T.stripSuffix ".drv" noHash)
