-- | Turns successive raw readings into 'Sample's.
--
-- CPU for a run is computed from the sum, over its members, of own plus
-- reaped-children CPU.  When a member exits its time moves into its parent's
-- reaped-children figure, so the sum stays continuous and short-lived
-- processes (compiler invocations between samples) are not lost.  Disk and
-- network have no such roll-up, so they are differenced per process.
module Hydrometer.Agent.Sampler
  ( Platform (..)
  , SamplerState
  , initialState
  , takeSample
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as A
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import Hydrometer.Agent.Attribution
import Hydrometer.Agent.Sys
import Hydrometer.Types

data Platform = Platform
  { plReadHost     :: IO HostRaw
  , plReadProcs    :: IO [Proc]
  , plReadProcArgs :: Int -> IO (Maybe ProcArgs)
  , plNetBytes     :: IO (IntMap (Int64, Int64))
    -- ^ Cumulative (rx, tx) per pid, where the platform can tell.
  , plIsBuildUid   :: Int -> Bool
  , plSelfPid      :: Int
  }

-- | Per-process cumulative counters from the previous sample.
data PrevProc = PrevProc
  { ppStart :: !Int64
  , ppCpu   :: !Int64   -- own CPU ns
  , ppRead  :: !Int64
  , ppWrite :: !Int64
  }

data SamplerState = SamplerState
  { ssTime     :: !EpochMs
  , ssHost     :: !(Maybe HostRaw)
  , ssProcs    :: !(IntMap PrevProc)
  , ssNet      :: !(IntMap (Int64, Int64))
  , ssRunCpu   :: !(Map Text Int64)
    -- ^ Run key -> cumulative CPU ns (own + reaped children, all members).
  , ssIdent    :: !(Map Text (Maybe RootInfo, Int))
    -- ^ Run key -> identification, and how many attempts it took.  Failed
    -- identifications are retried a few times (build.json can appear after
    -- the process does).
  }

initialState :: SamplerState
initialState = SamplerState 0 Nothing IM.empty IM.empty M.empty M.empty

runKeyFor :: RunKind -> Proc -> Text
runKeyFor k p = runKindText k <> ":" <> T.pack (show (pPid p)) <> ":" <> T.pack (show (pStartMs p))

clamp0 :: Int64 -> Int64
clamp0 = max 0

takeSample :: Platform -> Int64 -> EpochMs -> SamplerState -> IO (Maybe Sample, SamplerState)
takeSample pl seqNo now st = do
  host <- plReadHost pl
  procs <- plReadProcs pl
  net <- plNetBytes pl
  let g = groupProcs (plIsBuildUid pl) procs
      byPid = IM.fromList [ (pPid p, p) | p <- procs ]
      prevT = ssTime st
      -- Delta of a per-process counter; a process first seen since the last
      -- sample contributes everything it has done, an old one we never saw
      -- (the agent just started) contributes nothing.
      fresh p = prevT > 0 && pStartMs p >= prevT
      procDelta sel selP p = case IM.lookup (pPid p) (ssProcs st) of
        Just pp | ppStart pp == pStartMs p -> clamp0 (sel p - selP pp)
        _ | fresh p -> sel p
          | otherwise -> 0
      netDelta p = case (IM.lookup (pPid p) net, IM.lookup (pPid p) (ssNet st)) of
        (Just (r, t), Just (r0, t0)) -> (clamp0 (r - r0), clamp0 (t - t0))
        (Just (r, t), Nothing) | fresh p -> (r, t)
        _ -> (0, 0)

  -- Identify new runs.
  idents <- fmap M.fromList $ mapM (identify st) (gRoots g)
  let runs =
        [ (key, r, k, ms, ident)
        | (r, k) <- gRoots g
        , let key = runKeyFor k r
        , let ms = IM.findWithDefault [r] (pPid r) (gMembers g)
        , let ident = fst (M.findWithDefault (Nothing, 0) key idents)
        ]
      runCpuNow = M.fromList [ (key, sum [ pCpuNs p + pChildCpuNs p | p <- ms ]) | (key, _, _, ms, _) <- runs ]
      runStats =
        [ RunStats
            { rKey = key
            , rKind = maybe k riKind ident
            , rName = maybe (pComm r) riName ident
            , rOutputs = maybe [] riOutputs ident
            , rSystem = ident >>= riSystem
            , rStartTime = pStartMs r
            , rCpuMs = (`div` 1000000) $ case M.lookup key (ssRunCpu st) of
                Just c0 -> clamp0 (cpuNow - c0)
                Nothing | fresh r -> cpuNow
                        | otherwise -> 0
            , rRss = sum (map pRss ms)
            , rFootprint = sum (map pFootprint ms)
            , rDiskReadBytes = sum (map (procDelta pDiskRead ppRead) ms)
            , rDiskWriteBytes = sum (map (procDelta pDiskWrite ppWrite) ms)
            , rNetRxBytes = sum (map (fst . netDelta) ms)
            , rNetTxBytes = sum (map (snd . netDelta) ms)
            , rProcs = length ms
            }
        | (key, r, k, ms, ident) <- runs
        , let cpuNow = M.findWithDefault 0 key runCpuNow
        ]
      classes = M.elems $ M.fromListWith addClass
        [ (c, ClassStats
                { cClass = c
                , cCpuMs = procDelta pCpuNs ppCpu p `div` 1000000
                , cRss = pRss p
                , cDiskReadBytes = procDelta pDiskRead ppRead p
                , cDiskWriteBytes = procDelta pDiskWrite ppWrite p
                , cNetRxBytes = fst (netDelta p)
                , cNetTxBytes = snd (netDelta p)
                , cProcs = 1 })
        | p <- gOther g
        , let c = processClass (plSelfPid pl) byPid p
        ]
      addClass a b = a { cCpuMs = cCpuMs a + cCpuMs b, cRss = cRss a + cRss b
                       , cDiskReadBytes = cDiskReadBytes a + cDiskReadBytes b
                       , cDiskWriteBytes = cDiskWriteBytes a + cDiskWriteBytes b
                       , cNetRxBytes = cNetRxBytes a + cNetRxBytes b
                       , cNetTxBytes = cNetTxBytes a + cNetTxBytes b
                       , cProcs = cProcs a + cProcs b }
      sample = case ssHost st of
        Nothing -> Nothing
        Just h0 -> Just Sample
          { sSeq = seqNo
          , sTime = now
          , sHost = hostDelta (now - prevT) h0 host
          , sRuns = runStats
          , sClasses = classes
          }
      st' = SamplerState
        { ssTime = now
        , ssHost = Just host
        , ssProcs = IM.fromList [ (pPid p, PrevProc (pStartMs p) (pCpuNs p) (pDiskRead p) (pDiskWrite p)) | p <- procs ]
        , ssNet = net
        , ssRunCpu = runCpuNow
        , ssIdent = idents
        }
  pure (sample, st')
  where
    identify s (r, k) = do
      let key = runKeyFor k r
      case M.lookup key (ssIdent s) of
        Just (Just i, n) -> pure (key, (Just i, n))
        Just (Nothing, n) | n >= 6 -> pure (key, (Nothing, n))
        prev -> do
          i <- identifyRoot pl r k
          pure (key, (i, maybe 1 ((+ 1) . snd) prev))

identifyRoot :: Platform -> Proc -> RunKind -> IO (Maybe RootInfo)
identifyRoot pl r k = do
  margs <- plReadProcArgs pl (pPid r)
  case (k, margs) of
    (Evaluation, Just a) -> pure (identifyEval (paArgv a))
    (VmBuild, Just a) -> case filter (".json" `T.isSuffixOf`) (paArgv a) of
      (f : _) -> do
        v <- try (A.eitherDecodeFileStrict (T.unpack f)) :: IO (Either SomeException (Either String A.Value))
        pure $ case v of
          Right (Right j) -> identifyFromBuildJson j
          _ -> Nothing
      [] -> pure Nothing
    (_, Just a) -> pure (identifyFromEnv (paEnv a))
    (_, Nothing) -> pure Nothing

hostDelta :: Int64 -> HostRaw -> HostRaw -> HostStats
hostDelta interval a b = HostStats
  { hIntervalMs = interval
  , hCpuUserMs = d hrCpuUserMs
  , hCpuSystemMs = d hrCpuSystemMs
  , hCpuIdleMs = d hrCpuIdleMs
  , hLoad1 = l1, hLoad5 = l5, hLoad15 = l15
  , hMemTotal = hrMemTotal b
  , hMemUsed = hrMemUsed b
  , hMemCompressed = hrMemCompressed b
  , hMemPressure = hrMemPressure b
  , hCpuPressure = hrCpuPressure b
  , hIoPressure = hrIoPressure b
  , hSwapUsed = hrSwapUsed b
  , hSwapTotal = hrSwapTotal b
  , hPageouts = d hrPageouts
  , hNetRxBytes = d hrNetRx
  , hNetTxBytes = d hrNetTx
  , hDiskReadBytes = d hrDiskRead
  , hDiskWriteBytes = d hrDiskWrite
  , hOpenFiles = hrOpenFiles b
  , hMaxFiles = hrMaxFiles b
  , hFreeVnodes = hrFreeVnodes b
  , hMaxVnodes = hrMaxVnodes b
  , hStoreFree = hrStoreFree b
  , hStoreTotal = hrStoreTotal b
  }
  where
    d f = clamp0 (f b - f a)
    (l1, l5, l15) = hrLoad b
