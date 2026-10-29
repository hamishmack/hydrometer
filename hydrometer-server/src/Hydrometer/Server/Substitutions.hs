-- | Where an evaluation's substituted steps got their outputs, and how much
-- they downloaded.
--
-- A substitution step is the queue runner copying a derivation's outputs,
-- and whatever of their closure is missing, into this host's store from a
-- substituter.  Hydra records neither the substituter nor the size, so:
--
-- * Downloaded: the paths of the outputs' closure that were registered in
--   this store while the step ran (NAR sizes, so before compression).
--   Steps running at the same time can share a download; each is credited
--   with it.
-- * Source: peernix (the highest-priority substituter) logs which LAN peer
--   it found a path on.  Paths no peer had came from the first public cache
--   in priority order that has them (assuming it had them then too).
--
-- Worked out from this host's store and journal, so paths since garbage
-- collected, or older than the journal, show what is left.
module Hydrometer.Server.Substitutions
  ( SubstEnv
  , newSubstEnv
  , evalSubstitutions
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BC
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Scientific (toBoundedInteger)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)
import Network.Socket (AddrInfo (..), SockAddr (..), defaultHints, getAddrInfo, hostAddressToTuple, SocketType (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Hydrometer.Api (Table (..))

data SubstEnv = SubstEnv
  { seManager :: Manager
  , seCaches  :: [Text]
    -- ^ Public binary caches, in the order nix tries them.
  , seAgents  :: [(Text, Text)]
    -- ^ (machine name, agent URL): the URL's host names the peer.
  , seSource  :: TVar (Map Text Text)
    -- ^ Hash part -> the public cache a path came from.
  , seResults :: TVar (Map Int Table)
    -- ^ Per evaluation, once none of its substitutions is running.
  , seLock    :: MVar ()
  }

newSubstEnv :: Manager -> [Text] -> [(Text, Text)] -> IO SubstEnv
newSubstEnv m caches agents = SubstEnv m (map (T.dropWhileEnd (== '/')) caches) agents
  <$> newTVarIO M.empty <*> newTVarIO M.empty <*> newMVar ()

-- | The 300 longest substitution steps of an evaluation.
evalSubstitutions :: SubstEnv -> Connection -> Int -> IO Table
evalSubstitutions env c eid = do
  cached <- M.lookup eid <$> readTVarIO (seResults env)
  case cached of
    Just t -> pure t
    Nothing -> withMVar (seLock env) $ \_ -> do
      steps <- query c
        "SELECT s.build, s.stepnr, s.drvpath, hydrometer.drv_name(s.drvpath), s.starttime::bigint, s.stoptime::bigint, \
        \  s.busy <> 0, coalesce(array_agg(o.path) FILTER (WHERE o.path IS NOT NULL), '{}') \
        \FROM buildsteps s LEFT JOIN buildstepoutputs o ON o.build = s.build AND o.stepnr = s.stepnr \
        \WHERE s.type = 1 AND s.starttime IS NOT NULL \
        \  AND s.build IN (SELECT build FROM jobsetevalmembers WHERE eval = ?) \
        \GROUP BY s.build, s.stepnr \
        \ORDER BY coalesce(s.stoptime, extract(epoch FROM now())::int) - s.starttime DESC LIMIT 300"
        (Only eid) :: IO [(Int, Int, Text, Text, Int64, Maybe Int64, Bool, PGArray Text)]
      t <- compute [ (b, n, d, nm, st, sp, busy, outs) | (b, n, d, nm, st, sp, busy, PGArray outs) <- steps ]
      let running = or [ busy | (_, _, _, _, _, _, busy, _) <- steps ]
      if running then pure () else atomically $ modifyTVar' (seResults env) $ \m ->
        M.insert eid t (if M.size m > 200 then M.empty else m)
      pure t
  where
    compute steps = do
      let outs = S.toList (S.fromList (concat [ o | (_, _, _, _, _, _, _, o) <- steps ]))
      info <- pathInfo outs
      let from = minimum (maxBound : [ st | (_, _, _, _, st, _, _, _) <- steps ])
          to = maximum (0 : [ fromMaybe st sp | (_, _, _, _, st, sp, _, _) <- steps ])
      peers <- if null steps then pure M.empty else peerFinds (from - 60) (to + 60)
      names <- peerNames (seAgents env)
      let closure o = go S.empty [o]
            where
              go seen [] = seen
              go seen (p : ps)
                | p `S.member` seen = go seen ps
                | otherwise = go (S.insert p seen) (maybe [] piRefs (M.lookup p info) ++ ps)
          perStep =
            [ (s, downloaded, cl)
            | s@(_, _, _, _, st, sp, _, os) <- steps
            , let cl = S.unions (map closure os)
                  inWindow p = case M.lookup p info >>= piRegistered of
                    Just r -> r >= st - 2 && r <= fromMaybe maxBound sp + 2
                    Nothing -> False
                  downloaded = filter inWindow (S.toList cl) ]
          needCache = S.toList (S.fromList [ hashPart p | (_, ds, _) <- perStep, p <- ds, not (M.member (hashPart p) peers) ])
      caches <- cacheSources needCache
      let source p = case M.lookup (hashPart p) peers of
            Just ip -> fromMaybe ip (M.lookup ip names)
            Nothing -> fromMaybe "unknown" (M.lookup (hashPart p) caches)
          size p = maybe 0 piNarSize (M.lookup p info)
      pure $ Table
        [ "build", "stepnr", "drv", "drv_name", "started", "stopped", "source", "downloaded_bytes"
        , "downloaded_paths", "from", "output_bytes", "closure_bytes", "closure_paths" ]
        [ [ A.toJSON b, A.toJSON n, A.toJSON (T.takeWhileEnd (/= '/') d), A.toJSON nm
          , A.toJSON (st * 1000), A.toJSON ((* 1000) <$> sp)
          , A.toJSON (case os of (o : _) | M.member o info -> Just (source o); _ -> Nothing)
          , A.toJSON (sum (map size ds)), A.toJSON (length ds)
          , A.toJSON (T.intercalate "; " [ src <> ": " <> T.pack (show k) <> (if k == 1 then " path " else " paths ") <> fmtBytes bytes
                                         | (src, (k, bytes)) <- sortOn (Down . snd . snd) (M.toList (M.fromListWith add [ (source p, (1 :: Int, size p)) | p <- ds ])) ])
          , A.toJSON (sum (map size os)), A.toJSON (sum (map size (S.toList cl))), A.toJSON (S.size cl) ]
        | ((b, n, d, nm, st, sp, _, os), ds, cl) <- perStep ]
    add (a, b) (x, y) = (a + x, b + y)

    -- Hash parts of paths peernix found on a peer, and the peer's address.
    peerFinds :: Int64 -> Int64 -> IO (Map Text Text)
    peerFinds from to = do
      r <- try (readProcessWithExitCode "journalctl"
             [ "--no-pager", "--output=cat", "--unit=peernix", "--grep=Found .* at peer"
             , "--since=@" <> show from, "--until=@" <> show to ] "") :: IO (Either SomeException (ExitCode, String, String))
      pure $ case r of
        Right (_, out, _) -> M.fromList
          [ (h, ip) | l <- T.lines (T.pack out)
                    , ("Found" : h : "at" : "peer" : ip : _) <- [dropWhile (/= "Found") (T.words l)] ]
        Left _ -> M.empty

    -- The first public cache, in priority order, that has each path.
    cacheSources :: [Text] -> IO (Map Text Text)
    cacheSources hs = do
      known <- readTVarIO (seSource env)
      let todo = filter (`M.notMember` known) hs
      found <- concat <$> mapM (mapConcurrently lookupOne) (chunks 16 todo)
      atomically $ modifyTVar' (seSource env) $ \m ->
        M.union (M.fromList [ (h, src) | (h, Just src) <- found ]) (if M.size m > 100000 then M.empty else m)
      m <- readTVarIO (seSource env)
      pure (M.fromList [ (h, src) | h <- hs, Just src <- [M.lookup h m] ])
    lookupOne h = (,) h <$> firstJustM [ c | c <- seCaches env ] (\cache -> do
      ok <- has cache h
      pure (if ok then Just (T.replace "https://" "" cache) else Nothing))
    has cache h = do
      r <- try $ do
        req <- parseRequest (T.unpack ("HEAD " <> cache <> "/" <> h <> ".narinfo"))
        statusCode . responseStatus <$> httpNoBody req { responseTimeout = responseTimeoutMicro 10000000 } (seManager env)
      pure (either (const False :: SomeException -> Bool) (== 200) r)

    chunks _ [] = []
    chunks k xs = let (a, b) = splitAt k xs in a : chunks k b
    firstJustM [] _ = pure Nothing
    firstJustM (x : xs) f = f x >>= maybe (firstJustM xs f) (pure . Just)

hashPart :: Text -> Text
hashPart = T.take 32 . T.takeWhileEnd (/= '/')

data PathInfo = PathInfo { piNarSize :: !Int64, piRefs :: ![Text], piRegistered :: !(Maybe Int64) }

-- | Size, references and registration time of the store paths, and of
-- their closures.  Paths no longer in the store are left out.
pathInfo :: [Text] -> IO (Map Text PathInfo)
pathInfo [] = pure M.empty
pathInfo paths = do
  (_, invalid, _) <- readProcessWithExitCode "nix-store" ("--check-validity" : "--print-invalid" : map T.unpack paths) ""
  let bad = S.fromList (T.lines (T.pack invalid))
      valid = filter (`S.notMember` bad) paths
  if null valid then pure M.empty else do
    (_, out, _) <- readProcessWithExitCode "nix" (["path-info", "--json", "--recursive"] ++ map T.unpack valid) ""
    pure $ case A.decodeStrict (BC.pack out) :: Maybe A.Object of
      Just o -> M.fromList [ (K.toText k, i) | (k, A.Object v) <- KM.toList o, Just i <- [entry v] ]
      Nothing -> M.empty
  where
    entry v = do
      sz <- int =<< KM.lookup "narSize" v
      let refs = case KM.lookup "references" v of
            Just (A.Array a) -> [ r | A.String r <- foldr (:) [] a ]
            _ -> []
      pure (PathInfo sz refs (int =<< KM.lookup "registrationTime" v))
    int (A.Number n) = fromIntegral <$> (toBoundedInteger n :: Maybe Int)
    int _ = Nothing

-- | The agents' machines by IPv4 address.
peerNames :: [(Text, Text)] -> IO (Map Text Text)
peerNames agents = M.fromList . concat <$> mapM one agents
  where
    one (name, url) = do
      let host = T.takeWhile (\ch -> ch /= ':' && ch /= '/') (T.drop 3 (snd (T.breakOn "://" url)))
      r <- try (getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just (T.unpack host)) Nothing) :: IO (Either SomeException [AddrInfo])
      pure [ (ip, name) | Right as <- [r], a <- as, SockAddrInet _ h <- [addrAddress a]
                        , let (w, x, y, z) = hostAddressToTuple h
                              ip = T.intercalate "." (map (T.pack . show) [w, x, y, z]) ]

fmtBytes :: Int64 -> Text
fmtBytes b
  | b >= 1024 ^ (3 :: Int) = T.pack (show1 (fromIntegral b / 1024 ^ (3 :: Int))) <> " GiB"
  | b >= 1024 ^ (2 :: Int) = T.pack (show1 (fromIntegral b / 1024 ^ (2 :: Int))) <> " MiB"
  | otherwise = T.pack (show (b `div` 1024)) <> " KiB"
  where show1 x = show (fromIntegral (round (x * 10 :: Double) :: Int) / 10 :: Double)
