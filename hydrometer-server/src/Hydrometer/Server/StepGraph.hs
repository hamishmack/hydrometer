-- | The steps of an evaluation and which of them each one waited for.
--
-- Hydra records steps, not the dependencies between them, so these come
-- from the .drv files: a step depends on the nearest steps found by
-- following its derivation's inputs (derivations that ran no step, because
-- their outputs were already valid, are walked through).
module Hydrometer.Server.StepGraph
  ( GraphEnv
  , newGraphEnv
  , evalGraph
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (foldM, forM)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
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
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (..), query)

import Hydrometer.Server.Drv

data GraphEnv = GraphEnv
  { geInputs  :: TVar (Map Text [Text])
    -- ^ .drv path -> its input derivations.  Derivations do not change;
    -- the map is cleared when it grows.
  , geResults :: TVar (Map Int (UTCTime, Bool, A.Value))
    -- ^ Per evaluation: when computed, whether every step had finished.
  , geLock    :: MVar ()
    -- ^ One walk at a time.
  }

newGraphEnv :: IO GraphEnv
newGraphEnv = GraphEnv <$> newTVarIO M.empty <*> newTVarIO M.empty <*> newMVar ()

-- | Steps shown at most: the longest ones, so the chart stays readable.
maxNodes :: Int
maxNodes = 300

-- | Derivations read at most per walk (haskell.nix closures are large).
maxReads :: Int
maxReads = 60000

data Node = Node
  { nDrv :: !Text, nName :: !Text, nStart :: !Int64, nStop :: !(Maybe Int64)
  , nBusy :: !Bool, nStatus :: !(Maybe Int), nSubstituted :: !Bool
  , nMachine :: !Text, nBuild :: !Int }

-- | Cached for two minutes, or ten once every step has finished.
evalGraph :: GraphEnv -> Connection -> Int -> IO A.Value
evalGraph env c eid = do
  now <- getCurrentTime
  cached <- M.lookup eid <$> readTVarIO (geResults env)
  case cached of
    Just (t, done, r) | diffUTCTime now t < (if done then 600 else 120) -> pure r
    _ -> withMVar (geLock env) $ \_ -> do
      (done, r) <- compute
      atomically $ modifyTVar' (geResults env) (M.insert eid (now, done, r) . M.filter (\(t, _, _) -> diffUTCTime now t < 900))
      pure r
  where
    compute = do
      -- One node per derivation (a substitution has a step per output).
      rows <- query c
        "SELECT s.drvpath, hydrometer.drv_name(s.drvpath), min(s.starttime)::bigint * 1000, \
        \  CASE WHEN bool_or(s.busy <> 0) THEN NULL ELSE max(s.stoptime)::bigint * 1000 END, \
        \  bool_or(s.busy <> 0), max(s.status), bool_and(s.type = 1), max(s.machine), min(s.build) \
        \FROM buildsteps s \
        \WHERE s.build IN (SELECT build FROM jobsetevalmembers WHERE eval = ?) AND s.starttime IS NOT NULL \
        \GROUP BY s.drvpath"
        (Only eid)
      let allNodes = [ Node d nm st sp busy status subst (fromMaybe "" m) b
                     | (d, nm, Just st, sp, busy, status, subst, m, b) <- rows ]
          dur n = fromMaybe maxBound (nStop n) - nStart n
          kept = take maxNodes (sortOn (Down . dur) allNodes)
          keptSet = S.fromList (map nDrv kept)
      readCount <- newIORef (0 :: Int)
      memo <- newIORef (M.empty :: Map Text (Set Text))
      let inputsOf d = do
            known <- M.lookup d <$> readTVarIO (geInputs env)
            case known of
              Just is -> pure is
              Nothing -> do
                n <- readIORef readCount
                if n >= maxReads then pure [] else do
                  modifyIORef' readCount (+ 1)
                  is <- maybe [] dInputs <$> readDrv d
                  atomically $ modifyTVar' (geInputs env) $ \m ->
                    M.insert d is (if M.size m > 200000 then M.empty else m)
                  pure is
          -- The kept steps reachable from @d@ without passing through one.
          reach d
            | d `S.member` keptSet = pure (S.singleton d)
            | otherwise = below d
          below d = do
            seen <- M.lookup d <$> readIORef memo
            case seen of
              Just r -> pure r
              Nothing -> do
                is <- inputsOf d
                r <- foldM (\acc i -> S.union acc <$> reach i) S.empty is
                modifyIORef' memo (M.insert d r)
                pure r
      -- A substitution fetches outputs from a cache: it waits for none of
      -- its derivation's inputs.
      deps <- forM (filter (not . nSubstituted) kept) $ \n -> (,) (nDrv n) <$> below (nDrv n)
      nRead <- readIORef readCount
      let index = M.fromList (zip (map nDrv kept) [0 :: Int ..])
          edges = [ [i, j] | (d, ds) <- deps, Just j <- [M.lookup d index], dep <- S.toList ds, Just i <- [M.lookup dep index] ]
          done = all (\n -> not (nBusy n)) allNodes
      pure (done, A.object
        [ "nodes" .= [ A.object
            [ "name" .= nName n, "drv" .= T.takeWhileEnd (/= '/') (nDrv n), "start" .= nStart n, "stop" .= nStop n
            , "busy" .= nBusy n, "status" .= nStatus n
            , "kind" .= (if nSubstituted n then "substituted" else "built" :: Text)
            , "machine" .= nMachine n, "build" .= nBuild n ]
          | n <- kept ]
        , "edges" .= edges
        , "steps" .= length allNodes
        , "truncated" .= (nRead >= maxReads)
        ])
