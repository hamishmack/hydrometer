-- | "What is the farm doing right now": hydra's busy steps joined with the
-- agents' latest samples and the queue runner's view.
module Hydrometer.Server.Now
  ( buildNow
  ) where

import Control.Concurrent.STM
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple (Connection)

import Hydrometer.Api
import Hydrometer.Server.Db (epochMs)
import Hydrometer.Server.Ingest (AgentState (..))
import Hydrometer.Server.Queries (busySteps, lastStepFinish, pendingEvals, runningEvals)
import Hydrometer.Server.QueueRunner
import Hydrometer.Types

cpuUtil :: HostStats -> Maybe Double
cpuUtil h =
  let busy = hCpuUserMs h + hCpuSystemMs h
      total = busy + hCpuIdleMs h
  in if total <= 0 then Nothing else Just (fromIntegral busy / fromIntegral total)

perSecond :: HostStats -> Int64 -> Maybe Double
perSecond h v = if hIntervalMs h <= 0 then Nothing else Just (fromIntegral v * 1000 / fromIntegral (hIntervalMs h))

buildNow :: Connection -> TVar (Map Text AgentState) -> TVar QueueState -> IO NowResponse
buildNow c agentsVar queueVar = do
  agents <- readTVarIO agentsVar
  qs <- readTVarIO queueVar
  now <- epochMs <$> getCurrentTime
  steps <- busySteps c
  evals <- runningEvals c
  pending <- pendingEvals c
  lastFinish <- lastStepFinish c
  let qrByName = M.fromList [ (qmName m, m) | m <- qsMachines qs ]
      stepsByMachine = M.fromListWith (++) [ (m, [s]) | s@(m, _, _, _, _, _, _, _, _, _) <- steps ]
      names = S.toList (S.fromList (M.keys agents ++ M.keys qrByName ++ filter (not . T.null) (M.keys stepsByMachine)))
      -- When the queue runner's job lists can be trusted to judge hydra's
      -- busy steps against: a recent, successful poll.
      qrAsOf = case (qsPolled qs, qsError qs) of
        (Just t, Nothing) | let ms = round (utcTimeToPOSIXSeconds t * 1000), now - ms < 60000 -> Just ms
        _ -> Nothing
      machines = map (machineNow agents qrByName qrAsOf stepsByMachine) names
      queues = [ QueueNow sys n w | (sys, n, w) <- qsQueues qs, n > 0 || sys `elem` activeSystems ]
      activeSystems = concatMap mSystems machines
      evalRuns = [ (rName r, r, h) | AgentState { asLatest = Just s } <- M.elems agents
                                   , let h = sHost s, r <- sRuns s, rKind r == Evaluation ]
      evalsNow =
        [ EvalNow
            { eProject = p, eJobset = j, eStarted = st
            , eCores = (\(_, r, h) -> fromIntegral (rCpuMs r) / fromIntegral (max 1 (hIntervalMs h))) <$> er
            , eRss = (\(_, r, _) -> rFootprint r) <$> er
            , eLastEval = prev }
        | (p, j, st, prev) <- evals
        , let er = listToMaybe [ x | x@(n, _, _) <- evalRuns, n == p <> ":" <> j ] ]
      pendingNow =
        [ PendingEval { peProject = p, peJobset = j, peRequested = rq, peLastChecked = lc
                      , peLastEval = le, peLastEvalSeconds = ls }
        | (p, j, rq, lc, le, ls) <- pending ]
      -- How long the longest-waiting requested evaluation has waited.
      oldestRequest = case [ now - t | Just t <- map peRequested pendingNow ] of
        [] -> Nothing
        ws -> Just (maximum ws)
      runnableTotal = sum [ n | QueueNow _ n _ <- queues ]
      freeSlots = sum [ max 0 (fromMaybe 0 (mMaxJobs m) - length (filter (not . slOrphaned) (mSlots m))) | m <- machines, not (mDisabled m) ]
      orphans = [ (mName m, s) | m <- machines, s <- mSlots m, slOrphaned s ]
      stalled = case lastFinish of
        Just lf -> runnableTotal > 0 && freeSlots > 0 && now - lf > 15 * 60000
        Nothing -> False
      alerts =
        [ Alert "critical" ("No build step has finished for " <> T.pack (show ((now - fromMaybe now lastFinish) `div` 60000))
            <> " minutes while " <> T.pack (show runnableTotal) <> " steps are runnable and machines have free slots: the queue runner may be stalled.")
        | stalled ]
        ++ [ Alert "critical" (T.pack (show (length orphans)) <> " build steps that hydra lists as running are not running on any builder ("
              <> T.intercalate ", " [ n <> ": " <> T.pack (show k) | (n, k) <- M.toList (M.fromListWith (+) [ (n, 1 :: Int) | (n, _) <- orphans ]) ]
              <> "), so " <> T.pack (show (S.size (S.fromList (mapMaybe (slBuild . snd) orphans))))
              <> " builds wait on them. The queue runner lost them (usually when a builder reconnected); restarting hydra-queue-runner requeues them.")
           | not (null orphans) ]
        ++ [ Alert "critical" (T.pack (show (length pendingNow)) <> " evaluations are pending, the oldest requested "
              <> T.pack (show (w `div` 60000)) <> " minutes ago, but none is running: hydra-evaluator may be stalled.")
           | null evals, Just w <- [oldestRequest], w > 10 * 60000 ]
        ++ [ Alert "warning" ("Queue runner status unavailable: " <> e) | Just e <- [qsError qs] ]
        ++ [ Alert "warning" ("Agent on " <> n <> " is not answering: " <> T.take 200 e)
           | (n, a) <- M.toList agents, Just e <- [asError a] ]
        ++ [ Alert "warning" (mName m <> " is using " <> pct o mx <> " of its file descriptors (kern.maxfiles)")
           | m <- machines, Just o <- [mOpenFiles m], Just mx <- [mMaxFiles m], mx > 0, fromIntegral o > (0.8 :: Double) * fromIntegral mx ]
        ++ [ Alert "warning" (mName m <> " has only " <> pct f t <> " of its nix store disk free")
           | m <- machines, Just f <- [mStoreFree m], Just t <- [mStoreTotal m], t > 0, fromIntegral f < (0.1 :: Double) * fromIntegral t ]
        ++ [ Alert "warning" (mName m <> " is under memory pressure (" <> T.pack (show (round p :: Int)) <> ")")
           | m <- machines, Just p <- [mMemPressure m], p >= 50 ]
      pct a b = T.pack (show (a * 100 `div` max 1 b)) <> "%"
  pure NowResponse
    { nTime = now, nMachines = machines, nQueues = sortOn qSystem queues
    , nEvals = evalsNow, nPendingEvals = pendingNow, nAlerts = alerts }

machineNow
  :: Map Text AgentState -> Map Text QrMachine -> Maybe Int64
  -> Map Text [(Text, Int, Int, Text, Text, Text, Text, Text, Maybe Text, Maybe Int64)]
  -> Text -> MachineNow
machineNow agents qrByName qrAsOf stepsByMachine name = MachineNow
  { mName = name
  , mOs = aOs <$> info
  , mCpuCount = aCpuCount <$> info
  , mMemTotal = aMemTotal <$> info
  , mLastSample = sTime <$> latest
  , mCpuUtil = host >>= cpuUtil
  , mLoad1 = hLoad1 <$> host
  , mMemUsed = hMemUsed <$> host
  , mMemPressure = host >>= hMemPressure
  , mSwapUsed = hSwapUsed <$> host
  , mNetRxRate = host >>= \h -> perSecond h (hNetRxBytes h)
  , mNetTxRate = host >>= \h -> perSecond h (hNetTxBytes h)
  , mDiskReadRate = host >>= \h -> perSecond h (hDiskReadBytes h)
  , mDiskWriteRate = host >>= \h -> perSecond h (hDiskWriteBytes h)
  , mOpenFiles = host >>= hOpenFiles
  , mMaxFiles = host >>= hMaxFiles
  , mStoreFree = host >>= hStoreFree
  , mStoreTotal = host >>= hStoreTotal
  , mSystems = systems
  , mMaxJobs = maxJobs
  , mDisabled = maybe False qmDisabled qr
  , mClasses = M.fromList
      [ (cClass k, fromIntegral (cCpuMs k) / fromIntegral (max 1 (hIntervalMs h)))
      | Just s <- [latest], let h = sHost s, k <- sClasses s, cCpuMs k > 0 ]
  , mSlots = slots
  , mCpuHistory = reverse (mapMaybe cpuUtil (maybe [] asRecent agent))
  }
  where
    agent = M.lookup name agents
    info = agent >>= asInfo
    latest = agent >>= asLatest
    host = sHost <$> latest
    qr = M.lookup name qrByName
    builders = maybe [] aBuilders info
    -- The queue runner knows every registration's capacity; the agent's
    -- guess from builder command lines is only a fallback.
    systems = case qr of
      Just m | not (null (qmSystems m)) -> qmSystems m
      _ -> concatMap bSystems builders
    maxJobs = case qr of
      Just m -> Just (qmMaxJobs m)
      Nothing | null builders -> Nothing
              | otherwise -> Just (sum (map bMaxJobs builders))
    qrJobs = S.fromList (maybe [] qmJobs qr)
    -- Hydra lists the step as running here but the queue runner has no such
    -- job.  Only steps that started well before the poll count, since a
    -- step dispatched since the poll is not in its list yet.
    orphaned drvPath started = case qrAsOf of
      Just asOf -> not (T.takeWhileEnd (/= '/') drvPath `S.member` qrJobs)
                   && maybe False (< asOf - 120000) started
      Nothing -> False
    hydraSteps = M.findWithDefault [] name stepsByMachine
    runs = maybe [] sRuns latest
    interval = maybe 5000 (hIntervalMs . sHost) latest
    runByName = M.fromListWith (++) [ (rName r, [r]) | r <- runs, rKind r /= Evaluation ]
    stepSlot (_, b, sn, proj, js, job, drvName, drvPath, sys, started) =
      let r = M.lookup drvName runByName >>= listToMaybe
      in SlotNow
        { slDrvName = drvName, slDrvPath = Just drvPath
        , slKind = runKindText . rKind <$> r, slSystem = sys
        , slBuild = Just b, slStepNr = Just sn, slJob = Just job, slJobset = Just js, slProject = Just proj
        , slStarted = started
        , slCores = (\x -> fromIntegral (rCpuMs x) / fromIntegral (max 1 interval)) <$> r
        , slRss = rFootprint <$> r
        , slNetRxRate = (\x -> fromIntegral (rNetRxBytes x) * 1000 / fromIntegral (max 1 interval)) <$> r
        , slCpuSeconds = Nothing
        , slOrphaned = orphaned drvPath started && null r }
    stepNames = S.fromList [ d | (_, _, _, _, _, _, d, _, _, _) <- hydraSteps ]
    -- Builds the agent sees that hydra did not schedule here (IFD for an
    -- evaluation, or steps hydra has not yet recorded).
    extraSlots =
      [ SlotNow
          { slDrvName = rName r, slDrvPath = Nothing, slKind = Just (runKindText (rKind r))
          , slSystem = rSystem r, slBuild = Nothing, slStepNr = Nothing, slJob = Nothing
          , slJobset = Nothing, slProject = Nothing, slStarted = Just (rStartTime r)
          , slCores = Just (fromIntegral (rCpuMs r) / fromIntegral (max 1 interval))
          , slRss = Just (rFootprint r)
          , slNetRxRate = Just (fromIntegral (rNetRxBytes r) * 1000 / fromIntegral (max 1 interval))
          , slCpuSeconds = Nothing, slOrphaned = False }
      | r <- runs, rKind r /= Evaluation, not (rName r `S.member` stepNames) ]
    slots = map stepSlot hydraSteps ++ extraSlots
