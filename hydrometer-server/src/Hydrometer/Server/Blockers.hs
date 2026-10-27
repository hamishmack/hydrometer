{-# LANGUAGE MultiWayIf #-}
-- | Why work is not moving: what an evaluation's unfinished builds wait on,
-- and why runnable steps are not being handed to builders.
--
-- The queue runner exposes its queues but not its dependency graph, so the
-- graph comes from the .drv files: each unfinished build's inputs are
-- followed until they reach a step the queue runner or hydra knows about (the
-- "frontier": running, handed to a builder, orphaned, or runnable and
-- waiting).  Everything behind a frontier step waits for it.
module Hydrometer.Server.Blockers
  ( BlockerEnv
  , newBlockerEnv
  , evalBlockers
  , waitingAlerts
  , whyNotDispatched
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (forM)
import Data.IORef
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Numeric (showFFloat)

import Hydrometer.Api
import Hydrometer.Server.Drv
import Hydrometer.Server.Queries (busySteps)
import Hydrometer.Server.QueueRunner

data BlockerEnv = BlockerEnv
  { beFeatures :: TVar (Map Text (Text, [Text]))
    -- ^ .drv basename -> (system, required features).  Derivations do not
    -- change, so entries never go stale; the map is cleared when it grows.
  , beResults  :: TVar (Map Int (UTCTime, EvalBlockers))
  , beLock     :: MVar ()
    -- ^ One dependency walk at a time.
  }

newBlockerEnv :: IO BlockerEnv
newBlockerEnv = BlockerEnv <$> newTVarIO M.empty <*> newTVarIO M.empty <*> newMVar ()

stepFeatures :: BlockerEnv -> Text -> IO (Maybe (Text, [Text]))
stepFeatures env drv = do
  let key = T.takeWhileEnd (/= '/') drv
  cached <- M.lookup key <$> readTVarIO (beFeatures env)
  case cached of
    Just x -> pure (Just x)
    Nothing -> readDrv drv >>= \case
      Nothing -> pure Nothing
      Just d -> do
        let x = (dSystem d, dFeatures d)
        atomically $ modifyTVar' (beFeatures env) $ \m ->
          M.insert key x (if M.size m > 50000 then M.empty else m)
        pure (Just x)

-- | Why no builder takes a runnable step of @system@ that needs @features@,
-- or Nothing when some builder that could take it has capacity.
whyNotDispatched :: [QrRegistration] -> Text -> [Text] -> Maybe Text
whyNotDispatched regs system features
  | null able = Just ("no builder offers " <> system <> needing)
  | any usable able = Nothing
  | otherwise = Just ("only " <> T.intercalate ", " (map qrHost able) <> " can take "
      <> "them, and "
      <> (if length able == 1 then "it has" else "they have") <> " no capacity ("
      <> T.intercalate "; " [ qrHost r <> ": " <> noCapacity r | r <- able ] <> ")")
  where
    needing = if null features then "" else " with " <> T.intercalate ", " features
    able = [ r | r <- regs, system `elem` qrSystems r
               , all (`elem` (qrSupported r ++ qrMandatory r)) features
               , all (`elem` features) (qrMandatory r) ]
    usable r = qrHasCapacity r && not (qrDisabled r)
    noCapacity r = case concat
        [ [ "disabled" | qrDisabled r ]
        , [ "nix store " <> pct f <> " free, minimum " <> pct m | Just f <- [qrStoreFree r], Just m <- [qrStoreMin r], f < m ]
        , [ "build dir " <> pct f <> " free, minimum " <> pct m | Just f <- [qrBuildDirFree r], Just m <- [qrBuildDirMin r], f < m ]
        , [ "load " <> num l <> ", maximum " <> num m | Just l <- [qrLoad1 r], Just m <- [qrLoad1Max r], l > m ]
        , [ "all " <> T.pack (show (qrMaxJobs r)) <> " slots busy" | qrCurrentJobs r >= qrMaxJobs r ] ] of
      [] -> "no capacity"
      rs -> T.intercalate ", " rs
    pct x = T.pack (showFFloat (Just 0) x "") <> "%"
    num x = T.pack (showFFloat (Just 1) x "")

toMs :: UTCTime -> Int64
toMs t = round (utcTimeToPOSIXSeconds t * 1000)

-- | Alerts for runnable steps that have waited for a machine: grouped by
-- why no builder takes them.  Steps that some builder with capacity could
-- take are reported only after half an hour (dispatch should have happened).
waitingAlerts :: BlockerEnv -> QueueState -> UTCTime -> IO [Alert]
waitingAlerts env qs now = do
  let waiting = [ s | s <- qsSteps qs, qeWaiting s
                    , Just since <- [qeSince s], diffUTCTime now since > 300 ]
  reasons <- forM waiting $ \s -> do
    sf <- stepFeatures env (qeDrv s)
    let (sys, feats) = fromMaybe (qeSystem s, []) sf
        waited = maybe 0 (\t -> diffUTCTime now t) (qeSince s)
    pure $ case whyNotDispatched (qsRegistrations qs) sys feats of
      Just r -> Just ((sys, feats, r), waited)
      Nothing | waited > 1800 -> Just ((sys, feats, "builders that can take them have capacity, but the queue runner has not dispatched them"), waited)
              | otherwise -> Nothing
  let groups = M.fromListWith (\(n1, w1) (n2, w2) -> (n1 + n2, max w1 w2)) [ (k, (1 :: Int, w)) | Just (k, w) <- reasons ]
  pure
    [ Alert "warning" (T.pack (show n) <> " runnable " <> sys <> " steps"
        <> (if null feats then "" else " (needing " <> T.intercalate ", " feats <> ")")
        <> " have waited up to " <> fmtWait w <> ": " <> r)
    | ((sys, feats, r), (n, w)) <- M.toList groups ]
  where
    fmtWait w = let m = round w `div` 60 :: Int
                in if m >= 60 then T.pack (show (m `div` 60)) <> "h" <> T.pack (show (m `mod` 60)) <> "m"
                   else T.pack (show m) <> "m"

data Frontier = Frontier
  { fState   :: !Text
  , fMachine :: !(Maybe Text)
  , fSystem  :: !(Maybe Text)
  , fSince   :: !(Maybe Int64)
  }

-- | What an evaluation's unfinished builds wait on.  Cached for two minutes.
evalBlockers :: BlockerEnv -> Connection -> QueueState -> Int -> IO EvalBlockers
evalBlockers env c qs eid = do
  now <- getCurrentTime
  cached <- M.lookup eid <$> readTVarIO (beResults env)
  case cached of
    Just (t, r) | diffUTCTime now t < 120 -> pure r
    _ -> withMVar (beLock env) $ \_ -> do
      r <- compute now
      atomically $ modifyTVar' (beResults env) (M.insert eid (now, r) . M.filter (\(t, _) -> diffUTCTime now t < 600))
      pure r
  where
    compute now = do
      let nowMs = toMs now
      builds <- query c
        "SELECT b.id, b.drvpath FROM jobsetevalmembers m JOIN builds b ON b.id = m.build \
        \WHERE m.eval = ? AND b.finished = 0" (Only eid) :: IO [(Int, Text)]
      busy <- busySteps c
      let busyF = M.fromList
            [ (drv, Frontier (if | stepOrphaned nowMs qs machine drv started -> "orphaned"
                                 | stepUploading qs machine drv -> "uploading"
                                 | otherwise -> "building")
                             (Just machine) sys started)
            | (machine, _, _, _, _, _, _, drv, sys, started) <- busy ]
          queueF = M.fromList
            [ (storePath (qeDrv s), Frontier (if qeWaiting s then "waiting" else "scheduled") Nothing (Just (qeSystem s)) (toMs <$> qeSince s))
            | s <- qsSteps qs, qeWaiting s || qeScheduled s ]
          -- Hydra's busy rows say where a step runs; the queue knows the rest.
          frontier = M.union busyF queueF
      memo <- newIORef (M.empty :: Map Text (Set Text))
      readCount <- newIORef (0 :: Int)
      truncated <- newIORef False
      let limit = 400000
          visit :: Text -> IO (Set Text)
          visit drv
            | M.member drv frontier = pure (S.singleton drv)
            | otherwise = do
                known <- M.lookup drv <$> readIORef memo
                case known of
                  Just s -> pure s
                  Nothing -> do
                    n <- readIORef readCount
                    if n >= limit then writeIORef truncated True >> pure S.empty else do
                      writeIORef readCount (n + 1)
                      -- Mark before descending: a cycle (there should be
                      -- none) then ends here instead of looping.
                      modifyIORef' memo (M.insert drv S.empty)
                      inputs <- maybe [] dInputs <$> readDrv drv
                      s <- S.unions <$> mapM visit inputs
                      modifyIORef' memo (M.insert drv s)
                      pure s
      perBuild <- forM builds $ \(b, drv) -> (,) b . (,) drv <$> visit drv
      n <- readIORef readCount
      tr <- readIORef truncated
      let own drv = fState <$> M.lookup drv frontier
          count st = length [ () | (_, (drv, _)) <- perBuild, own drv `elem` map Just st ]
          blocked = length [ () | (_, (drv, s)) <- perBuild, own drv == Nothing, not (S.null s) ]
          held = M.fromListWith (+) [ (f, 1 :: Int) | (_, (_, s)) <- perBuild, f <- S.toList s ]
          top = take 40 (sortOn (Down . snd) (M.toList held))
      blockers <- forM top $ \(drv, k) -> do
        let f = frontier M.! drv
        sf <- stepFeatures env drv
        let sys = maybe (fSystem f) (Just . fst) sf
            feats = maybe [] snd sf
            reason = if fState f == "waiting" then sys >>= \s -> whyNotDispatched (qsRegistrations qs) s feats else Nothing
        pure Blocker
          { blDrvPath = drv, blName = drvName drv, blState = fState f, blMachine = fMachine f
          , blSystem = sys, blFeatures = feats, blSince = fSince f, blReason = reason, blBuilds = k }
      done <- getCurrentTime
      pure EvalBlockers
        { ebUnfinished = length builds
        , ebBuilding = count ["building", "scheduled", "uploading"]
        , ebOrphaned = count ["orphaned"]
        , ebWaiting = count ["waiting"]
        , ebBlocked = blocked
        , ebOther = length builds - count ["building", "scheduled", "uploading", "orphaned", "waiting"] - blocked
        , ebBlockers = blockers
        , ebComputedAt = toMs done
        , ebDurationMs = round (diffUTCTime done now * 1000)
        , ebDrvsRead = n
        , ebTruncated = tr }
    drvName p = let base = T.takeWhileEnd (/= '/') p
                    noHash = if T.length base > 33 && T.index base 32 == '-' then T.drop 33 base else base
                in fromMaybe noHash (T.stripSuffix ".drv" noHash)
