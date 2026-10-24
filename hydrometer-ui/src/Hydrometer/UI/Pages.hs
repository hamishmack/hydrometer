{-# LANGUAGE ScopedTypeVariables #-}
-- | The dashboard pages.
module Hydrometer.UI.Pages
  ( nowPage
  , machinePage
  , jobsPage
  , bottlenecksPage
  , evalsPage
  , evalPage
  , buildPage
  ) where

import Control.Monad (forM_, when, (<=<))
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Map.Strict qualified as M
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (toRealFloat)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Reflex.Dom.Core

import Hydrometer.Api
import Hydrometer.UI.Common

showT :: Show a => a -> Text
showT = T.pack . show

-- ---------------------------------------------------------------------------
-- Now

nowPage :: forall t m. W t m => m ()
nowPage = mdo
  nowD <- fetchDyn (Just 5) (constDyn "api/now") :: m (Dynamic t (Maybe NowResponse))
  clock <- clockDyn
  -- Machines whose orphaned-steps list is expanded.  Kept outside the
  -- rebuild below, which happens on every refresh.
  openD <- foldDyn (\k o -> if S.member k o then S.delete k o else S.insert k o) S.empty toggles
  toggles <- switchHold never <=< dyn $ ffor nowD $ \case
    Nothing -> loading >> pure never
    Just n -> do
      forM_ (nAlerts n) $ \a -> elClass "div" ("alert " <> alLevel a) (text (alMessage a))
      let ms = nMachines n
          busySlots = sum [ length (filter (not . slOrphaned) (mSlots m)) | m <- ms ]
          slotsTotal = sum [ fromMaybe 0 (mMaxJobs m) | m <- ms ]
          cores = sum [ fromIntegral (fromMaybe 0 (mCpuCount m)) * fromMaybe 0 (mCpuUtil m) | m <- ms ] :: Double
          totalCores = sum [ fromMaybe 0 (mCpuCount m) | m <- ms, isJust (mCpuUtil m) ]
          withAgent = length [ () | m <- ms, isJust (mLastSample m) ]
      elClass "div" "stats" $ do
        cpuGauge (if totalCores > 0 then Just (cores / fromIntegral totalCores) else Nothing)
          (showT (round cores :: Int) <> " / " <> showT totalCores <> " cores")
          (showT withAgent <> " of " <> showT (length ms) <> " machines reporting")
        statSub "Builds running" (showT busySlots <> " / " <> showT slotsTotal) (showT (max 0 (slotsTotal - busySlots)) <> " slots free")
        statSub "Network in" (fmtRate (sum [ fromMaybe 0 (mNetRxRate m) | m <- ms ]))
          ("out " <> fmtRate (sum [ fromMaybe 0 (mNetTxRate m) | m <- ms ]))
        -- One tile, not one per system: most systems have nothing waiting.
        let waiting = [ q | q <- nQueues n, qRunnable q > 0 ]
        statSub "Runnable" (showT (sum (map qRunnable (nQueues n))))
          (T.intercalate " · " [ qSystem q <> " " <> showT (qRunnable q)
                                   <> maybe "" (\w -> " (" <> fmtDuration (fromIntegral w / 1000) <> ")") (qOldestRunnableMs q)
                               | q <- waiting ])
        statSub "Evaluating" (showT (length (nEvals n)))
          (let k = length (nPendingEvals n) in if k == 0 then "" else showT k <> " pending")
      when (not (null (nEvals n) && null (nPendingEvals n))) $ do
        el "h3" (text "Evaluations")
        elClass "div" "grid" $ do
          forM_ (nEvals n) $ \e -> elClass "div" "card" $ do
            elClass "div" "title" $ do
              elClass "span" "name" (text (eProject e <> ":" <> eJobset e))
              elClass "span" "sub" $ dynText (ffor clock $ \c -> maybe "" (\s -> "running " <> fmtAgo c s) (eStarted e))
            elClass "div" "meters" $ do
              meterRow "CPU" ((/ 16) <$> eCores e) (maybe "" (\x -> fmtFixed 1 x <> " cores") (eCores e))
              meterRow "Mem" Nothing (maybe "" (fmtBytes . fromIntegral) (eRss e))
            elClass "div" "muted oneline" $ forM_ (eLastEval e) $ \i -> do
              text "previous: "
              elAttr "a" ("href" =: ("#/eval/" <> showT i)) (text ("eval " <> showT i))
          when (not (null (nPendingEvals n))) $ pendingCard clock (nPendingEvals n)
      el "h3" (text "Machines")
      elClass "div" "grid" $ leftmost <$> mapM (machineCard clock (current openD)) ms
  pure ()

-- | The farm's CPU use as a pressure gauge, in a stat tile two columns wide.
cpuGauge :: W t m => Maybe Double -> Text -> Text -> m ()
cpuGauge frac v sub = elAttr "div" ("class" =: "stat gauge" <> "title" =: ("CPU in use: " <> v)) $ do
  chartWidget "gauge-chart" "gauge" (constDyn (Just (frac, A.object [])))
  elClass "div" "text" $ do
    elClass "div" "k" (text "CPU in use")
    elClass "div" "v" (text (maybe "–" fmtPct frac))
    elClass "div" "s" (text v)
    elClass "div" "s" (text sub)

-- | The evaluator's queue, in the order it will start the evaluations.
pendingCard :: W t m => Dynamic t UTCTime -> [PendingEval] -> m ()
pendingCard clock ps = elClass "div" "card pending" $ do
  elClass "div" "title" $ do
    elClass "span" "name" (text "Pending")
    elClass "span" "sub" (text (showT (length ps) <> " waiting, in start order"))
  elClass "ol" "pending-list" $ forM_ ps $ \p -> el "li" $ do
    elAttr "div" ("class" =: "drv" <> "title" =: (peProject p <> ":" <> peJobset p)) $
      text (peProject p <> ":" <> peJobset p)
    elClass "div" "meta" $ do
      case peRequested p of
        Just t -> el "span" $ dynText (ffor clock (\c -> "requested " <> fmtAgo c t <> " ago"))
        Nothing -> el "span" (text "check interval elapsed")
      forM_ (peLastEvalSeconds p) $ \s -> el "span" (text ("last eval took " <> fmtDuration (fromIntegral s)))
      forM_ (peLastEval p) $ \i -> elAttr "a" ("href" =: ("#/eval/" <> showT i)) (text ("eval " <> showT i))

meterRow :: DomBuilder t m => Text -> Maybe Double -> Text -> m ()
meterRow label frac val = do
  elClass "span" "label" (text label)
  let f = maybe 0 (max 0 . min 1) frac
      cls = if f > 0.9 then "bar bad" else if f > 0.75 then "bar warn" else "bar"
  elClass "div" cls $ elAttr "div" ("style" =: ("width:" <> showT (round (f * 100) :: Int) <> "%")) blank
  elClass "span" "val" (text val)

-- | Returns the machine's name when its orphaned-steps list is toggled.
machineCard :: W t m => Dynamic t UTCTime -> Behavior t (S.Set Text) -> MachineNow -> m (Event t Text)
machineCard clock openB m = elClass "div" "card" $ do
  elClass "div" "title" $ do
    elClass "span" "name" $ elAttr "a" ("href" =: ("#/machine/" <> mName m)) (text (mName m))
    let flags = filter (not . T.null)
          [ if mDisabled m then "DISABLED" else ""
          , if isJust (mLastSample m) then "" else "no agent" ]
    elClass "span" "sub" $ text $ T.intercalate " · " $ filter (not . T.null)
      [ maybe "" (\c -> showT c <> " cores") (mCpuCount m)
      , maybe "" (fmtBytes . fromIntegral) (mMemTotal m) ]
    when (not (null flags)) $ elClass "span" "sub bad" (text (T.intercalate " · " flags))
  when (isJust (mLastSample m)) $ do
    elClass "div" "meters" $ do
      meterRow "CPU" (mCpuUtil m) (maybe "–" fmtPct (mCpuUtil m))
      let memFrac = (\u t -> fromIntegral u / fromIntegral t) <$> mMemUsed m <*> mMemTotal m
      meterRow "Mem" memFrac (maybe "–" (fmtBytes . fromIntegral) (mMemUsed m))
      meterRow "Press." ((/ 100) <$> mMemPressure m) (maybe "–" (showT . (round :: Double -> Int)) (mMemPressure m))
      meterRow "Net in" ((/ 125e6) <$> mNetRxRate m) (maybe "–" fmtRate (mNetRxRate m))
      meterRow "Disk w" ((/ 2e9) <$> mDiskWriteRate m) (maybe "–" fmtRate (mDiskWriteRate m))
      let fd = (\o x -> fromIntegral o / fromIntegral x) <$> mOpenFiles m <*> mMaxFiles m
      meterRow "Files" fd (maybe "–" showT (mOpenFiles m))
    when (not (null (mCpuHistory m))) $
      chartWidget "spark" "spark" (constDyn (Just (mCpuHistory m, A.object [])))
    -- Always present, so the card does not jump as other work comes and goes.
    let others = M.toList (M.filter (>= 0.05) (mClasses m))
        otherT = if null others then "" else
          "Other: " <> T.intercalate ", " [ k <> " " <> fmtFixed 1 v <> " cores" | (k, v) <- others ]
    elAttr "div" ("class" =: "muted oneline" <> "title" =: otherT) (text otherT)
  let (orphans, live) = partition slOrphaned (mSlots m)
  elClass "div" "slots" $ do
    forM_ live $ \s -> elClass "div" "slot" $ do
      elAttr "div" ("class" =: "drv" <> "title" =: fromMaybe (slDrvName s) (slDrvPath s)) $ do
        forM_ (slKind s) $ \k -> elClass "span" ("tag " <> (if isJust (slBuild s) then k else "ifd")) (text (if isJust (slBuild s) then k else "not a hydra step")) >> text " "
        case slBuild s of
          Just b -> elAttr "a" ("href" =: ("#/build/" <> showT b)) (text (slDrvName s))
          Nothing -> text (slDrvName s)
      elClass "div" "meta" $ do
        forM_ (slStarted s) $ \st -> el "span" $ dynText (ffor clock (`fmtAgo` st))
        forM_ (slCores s) $ \c -> el "span" (text (fmtFixed 1 c <> " cores"))
        forM_ (slRss s) $ \r -> el "span" (text (fmtBytes (fromIntegral r)))
        forM_ (slNetRxRate s) $ \r -> when (r > 1e5) $ el "span" (text ("↓" <> fmtRate r))
        forM_ (slJobset s) $ \j -> elAttr "span" ("class" =: "shrink" <> "title" =: j) (text j)
      forM_ (slJob s) $ \j -> elAttr "div" ("class" =: "job" <> "title" =: j)
        (text (if T.length j > 90 then "…" <> T.takeEnd 89 j else j))
    let free = fromMaybe 0 (mMaxJobs m) - length [ () | s <- live, isJust (slBuild s) ]
    when (free > 0) $ elClass "div" "slot free" $
      text (showT free <> " free slot" <> (if free == 1 then "" else "s"))
    -- Steps hydra lists as running that no builder is running (see the
    -- alert): collapsed, so they do not bury the real slots.
    if null orphans then pure never else do
      isOpen <- sample openB
      let openAttr = if S.member (mName m) isOpen then "open" =: "" else mempty
      elAttr "details" ("class" =: "orphans" <> openAttr) $ do
        (summary, _) <- el' "summary" $ text $
          showT (length orphans) <> " orphaned step" <> (if length orphans == 1 then "" else "s")
            <> ": hydra lists them as running, the queue runner does not"
        forM_ orphans $ \s -> elClass "div" "slot" $ do
          elAttr "div" ("class" =: "drv" <> "title" =: fromMaybe (slDrvName s) (slDrvPath s)) $
            case slBuild s of
              Just b -> elAttr "a" ("href" =: ("#/build/" <> showT b)) (text (slDrvName s))
              Nothing -> text (slDrvName s)
          elClass "div" "meta" $ do
            forM_ (slStarted s) $ \st -> el "span" $ dynText (ffor clock (\c -> "since " <> fmtAgo c st <> " ago"))
            forM_ (slJobset s) $ \j -> elAttr "span" ("class" =: "shrink" <> "title" =: j) (text j)
        pure (mName m <$ domEvent Click summary)

-- ---------------------------------------------------------------------------
-- Machine

machinePage :: forall t m. W t m => Text -> m ()
machinePage name = do
  el "h2" (text name)
  hoursD <- rangeSelector 6
  let q h = "?" <> rangeQuery h
  seriesD <- fetchDyn (Just 30) ((\h -> "api/machine/" <> name <> "/series" <> q h) <$> hoursD) :: m (Dynamic t (Maybe Series))
  stepsD <- fetchDyn (Just 30) ((\h -> "api/machine/" <> name <> "/steps" <> q h) <$> hoursD) :: m (Dynamic t (Maybe Table))
  elClass "div" "two" $ do
    chartWidget "chart" "line" $ ffor seriesD $ fmap $ \s -> (s, A.object
      [ "cols" .= (["cpu_util", "mem_frac"] :: [Text]), "unit" .= ("pct" :: Text), "yMax" .= (1 :: Int)
      , "labels" .= A.object ["cpu_util" .= ("CPU" :: Text), "mem_frac" .= ("memory used" :: Text)] ])
    chartWidget "chart" "line" $ ffor seriesD $ fmap $ \s -> (s, A.object
      [ "cols" .= (["net_rx_rate", "net_tx_rate", "disk_read_rate", "disk_write_rate"] :: [Text]), "unit" .= ("rate" :: Text) ])
  el "h3" (text "Build steps (click one for its build)")
  chartWidget "chart tall" "gantt" $ ffor stepsD $ fmap $ \t -> (ganttRows t, A.object [])
  el "h3" (text "Steps")
  tableWidget ["busy", "status"] stepsD

-- | Rows for charts.js gantt: one object per step.
ganttRows :: Table -> [A.Value]
ganttRows (Table cols rows) =
  [ A.object
      [ "start" .= num "started", "stop" .= num "stopped", "name" .= str "drv_name", "job" .= str "job"
      , "status" .= get "status", "busy" .= (num "busy" /= Just 0 && get "status" == A.Null)
      , "cpu" .= num "cpu_s", "rss" .= num "peak_rss", "href" .= ("#/build/" <> maybe "" (showT . (round :: Double -> Int)) (num "build")) ]
  | r <- rows
  , let get c = fromMaybe A.Null (M.lookup c (M.fromList (zip cols r)))
        num c = case get c of A.Number n -> Just (toRealFloat n :: Double); _ -> Nothing
        str c = case get c of A.String s -> s; _ -> ""
  ]

-- ---------------------------------------------------------------------------
-- Jobs

jobsPage :: forall t m. W t m => m ()
jobsPage = do
  let metrics = M.fromList [("cpu", "CPU hours"), ("wall", "wall hours"), ("rss", "peak memory"), ("net", "network in (GB)"), ("steps", "steps"), ("import", "import hours")] :: M.Map Text Text
  el "h2" (text "What uses the farm")
  elClass "p" "muted" (text "Finished build steps grouped by a property of the derivation or job. CPU and memory are measured by the agents (steps on machines without an agent count only towards wall time).")
  (groupD, metricD) <- elClass "div" "controls" $ do
    text "Group by"
    g <- dropdown "package" (constDyn (M.fromList [ (k, k) | k <- ["package", "component_kind", "component", "platform", "job_compiler", "project", "jobset", "job", "drv_name", "system", "machine", "run_kind"] ])) def
    text "Order by"
    mt <- dropdown "cpu" (constDyn metrics) def
    pure (value g, value mt)
  hoursD <- rangeSelector 24
  let url = (\g mt h -> "api/top?group=" <> g <> "&metric=" <> mt <> "&limit=40&" <> rangeQuery h) <$> groupD <*> metricD <*> hoursD
  tD <- fetchDyn Nothing url :: m (Dynamic t (Maybe Table))
  let barsOf mt (Table cols rows) =
        let col = case mt of
              "wall" -> "wall_h"; "rss" -> "max_peak_rss"; "net" -> "net_rx_gb"; "steps" -> "steps"; "import" -> "import_h"; _ -> "cpu_h"
            idx = length (takeWhile (/= col) cols)
        in [ (case r of (k : _) -> k; [] -> A.Null, case drop idx r of (v : _) -> v; [] -> A.Null) | r <- take 25 rows ]
  chartWidget "chart" "bars" $ (\mt -> fmap (\t -> (barsOf mt t, A.object
    [ "unit" .= (if mt == "rss" then "bytes" else "" :: Text), "label" .= fromMaybe mt (M.lookup mt metrics) ]))) <$> metricD <*> tD
  tableWidget [] tD

-- ---------------------------------------------------------------------------
-- Bottlenecks

bottlenecksPage :: forall t m. W t m => m ()
bottlenecksPage = do
  el "h2" (text "What limits the farm")
  elClass "p" "muted" (text "Each machine-minute is classified by what limited it: cpu (≥85% of cores busy), memory (pressure or paging), network, slots (every build slot busy without saturating the hardware: more slots or bigger VMs would help), scheduler (free slots while work for the machine's systems is runnable: the queue runner is not dispatching), starved (free slots, nothing runnable: waiting on dependencies elsewhere), idle.")
  hoursD <- rangeSelector 24
  bD <- fetchDyn (Just 60) ((\h -> "api/bottlenecks?" <> rangeQuery h) <$> hoursD) :: m (Dynamic t (Maybe A.Value))
  farmD <- fetchDyn (Just 60) ((\h -> "api/farm?" <> rangeQuery h) <$> hoursD) :: m (Dynamic t (Maybe Series))
  let series = (>>= A.parseMaybe (A.withObject "b" (A..: "series"))) <$> bD :: Dynamic t (Maybe Series)
      summary = (>>= A.parseMaybe (A.withObject "b" (A..: "summary"))) <$> bD :: Dynamic t (Maybe Table)
  chartWidget "chart tall" "line" $ ffor series $ fmap $ \s -> (s, A.object
    [ "cols" .= (["cpu", "memory", "network", "slots", "scheduler", "starved", "idle", "disabled"] :: [Text])
    , "stack" .= True, "unit" .= ("" :: Text) ])
  el "h3" (text "Farm")
  chartWidget "chart" "line" $ ffor farmD $ fmap $ \s -> (s, A.object
    [ "cols" .= (["cpu_util", "runs", "runnable_linux", "runnable_darwin"] :: [Text])
    , "axes" .= A.object ["runs" .= (1 :: Int), "runnable_linux" .= (1 :: Int), "runnable_darwin" .= (1 :: Int)]
    , "unit" .= ("pct" :: Text), "unit2" .= ("" :: Text)
    , "labels" .= A.object ["cpu_util" .= ("CPU (all machines)" :: Text), "runs" .= ("builds running" :: Text)] ])
  el "h3" (text "Minutes per machine")
  tableWidget [] summary

-- ---------------------------------------------------------------------------
-- Evaluations

evalsPage :: forall t m. W t m => m ()
evalsPage = do
  el "h2" (text "Evaluations")
  vD <- fetchDyn (Just 30) (constDyn "api/evals?limit=60") :: m (Dynamic t (Maybe A.Value))
  let evals = (>>= A.parseMaybe (A.withObject "e" (A..: "evals"))) <$> vD :: Dynamic t (Maybe Table)
  tableWidget [] evals

evalPage :: forall t m. W t m => Int -> m ()
evalPage eid = do
  el "h2" (text ("Evaluation " <> showT eid))
  vD <- fetchDyn (Just 30) (constDyn ("api/eval/" <> showT eid)) :: m (Dynamic t (Maybe A.Value))
  let field :: A.FromJSON a => A.Key -> Dynamic t (Maybe a)
      field k = (>>= A.parseMaybe (A.withObject "e" (A..: k))) <$> vD
      summaryD = field "summary" :: Dynamic t (Maybe Table)
  dyn_ $ ffor summaryD $ \case
    Just (Table cols (r : _)) -> do
      let get c = M.lookup c (M.fromList (zip cols r))
          numOf c = case get c of Just (A.Number n) -> Just (toRealFloat n :: Double); _ -> Nothing
          txt c = case get c of Just (A.String s) -> s; Just (A.Number n) -> fmtNum (toRealFloat n); _ -> "–"
      elClass "p" "muted" (text (txt "project" <> ":" <> txt "jobset" <> maybe "" (\e -> ", evaluated " <> fmtTime (round e)) (numOf "evaluated")))
      elClass "div" "stats" $ do
        stat "Builds" (txt "finished" <> " / " <> txt "builds")
        stat "Failed" (txt "failed")
        stat "Queued" (txt "queued")
        stat "Steps running" (txt "running_steps")
        stat "Span" (maybe "–" (\h -> fmtDuration (h * 3600)) (numOf "span_h"))
        stat "Step wall time" (maybe "–" (\h -> fmtNum h <> " h") (numOf "step_wall_h"))
        stat "…importing" (maybe "–" (\h -> fmtNum h <> " h") (numOf "import_h"))
        stat "…building" (maybe "–" (\h -> fmtNum h <> " h") (numOf "build_h"))
        stat "…uploading" (maybe "–" (\h -> fmtNum h <> " h") (numOf "upload_h"))
        stat "CPU (measured)" (maybe "–" (\h -> fmtNum h <> " h") (numOf "cpu_h"))
    Just _ -> elClass "div" "muted" (text "No such evaluation.")
    Nothing -> loading
  el "h3" (text "Timeline: steps running vs builds unfinished")
  elClass "p" "muted" (text "Where builds remain unfinished but few of this evaluation's steps run, it is waiting: for machines (see Bottlenecks), for dependencies being built for other evaluations, or for a stalled queue runner.")
  chartWidget "chart" "line" $ ffor (field "timeline" :: Dynamic t (Maybe Series)) $ fmap $ \s -> (s, A.object
    [ "cols" .= (["running_linux", "running_darwin", "unfinished_builds"] :: [Text])
    , "axes" .= A.object ["unfinished_builds" .= (1 :: Int)], "unit" .= ("" :: Text), "unit2" .= ("" :: Text) ])
  el "h3" (text "Longest steps")
  tableWidget ["busy"] (field "steps")

buildPage :: forall t m. W t m => Int -> m ()
buildPage bid = do
  el "h2" (text ("Build " <> showT bid))
  tD <- fetchDyn (Just 30) (constDyn ("api/build/" <> showT bid)) :: m (Dynamic t (Maybe Table))
  tableWidget [] tD
  el "h3" (text "Resource use of the last step")
  let lastStep = ffor tD $ \case
        Just (Table _ rows@(_ : _)) -> case last rows of
          (A.Number n : _) -> Just (round (toRealFloat n :: Double) :: Int)
          _ -> Nothing
        _ -> Nothing
  stepD <- fetchDyn Nothing ((\s -> "api/step/" <> showT bid <> "/" <> maybe "0" showT s <> "/series") <$> lastStep) :: m (Dynamic t (Maybe Series))
  chartWidget "chart" "line" $ ffor stepD $ fmap $ \s -> (s, A.object
    [ "cols" .= (["cores", "footprint"] :: [Text]), "axes" .= A.object ["footprint" .= (1 :: Int)]
    , "unit" .= ("cores" :: Text), "unit2" .= ("bytes" :: Text) ])

