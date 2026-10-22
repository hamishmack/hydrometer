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

import Control.Monad (forM_, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (toRealFloat)
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
nowPage = do
  nowD <- fetchDyn (Just 5) (constDyn "api/now") :: m (Dynamic t (Maybe NowResponse))
  clock <- clockDyn
  dyn_ $ ffor nowD $ \case
    Nothing -> loading
    Just n -> do
      forM_ (nAlerts n) $ \a -> elClass "div" ("alert " <> alLevel a) (text (alMessage a))
      let ms = nMachines n
          busySlots = sum (map (length . mSlots) ms)
          slotsTotal = sum [ fromMaybe 0 (mMaxJobs m) | m <- ms ]
          cores = sum [ fromIntegral (fromMaybe 0 (mCpuCount m)) * fromMaybe 0 (mCpuUtil m) | m <- ms ] :: Double
          totalCores = sum [ fromMaybe 0 (mCpuCount m) | m <- ms, isJust (mCpuUtil m) ]
      elClass "div" "stats" $ do
        stat "Builds running" (showT busySlots <> " / " <> showT slotsTotal)
        stat "CPU in use" (fmtNum cores <> " / " <> showT totalCores <> " cores")
        stat "Network in" (fmtRate (sum [ fromMaybe 0 (mNetRxRate m) | m <- ms ]))
        forM_ (nQueues n) $ \q ->
          stat ("Runnable " <> qSystem q) (showT (qRunnable q) <> maybe "" (\w -> " (" <> fmtDuration (fromIntegral w / 1000) <> ")") (qOldestRunnableMs q))
        stat "Evaluating" (showT (length (nEvals n)))
      when (not (null (nEvals n))) $ do
        el "h3" (text "Evaluations in progress")
        elClass "div" "grid" $ forM_ (nEvals n) $ \e -> elClass "div" "card" $ do
          elClass "div" "title" $ do
            elClass "span" "name" (text (eProject e <> ":" <> eJobset e))
            elClass "span" "sub" $ dynText (ffor clock $ \c -> maybe "" (\s -> "running " <> fmtAgo c s) (eStarted e))
          elClass "div" "meters" $ do
            meterRow "CPU" ((/ 16) <$> eCores e) (maybe "" (\x -> fmtNum x <> " cores") (eCores e))
            meterRow "Mem" Nothing (maybe "" (fmtBytes . fromIntegral) (eRss e))
          forM_ (eLastEval e) $ \i -> elClass "div" "muted" $ do
            text "previous: "
            elAttr "a" ("href" =: ("#/eval/" <> showT i)) (text ("eval " <> showT i))
      el "h3" (text "Machines")
      elClass "div" "grid" $ forM_ ms (machineCard clock)

meterRow :: DomBuilder t m => Text -> Maybe Double -> Text -> m ()
meterRow label frac val = do
  elClass "span" "label" (text label)
  let f = maybe 0 (max 0 . min 1) frac
      cls = if f > 0.9 then "bar bad" else if f > 0.75 then "bar warn" else "bar"
  elClass "div" cls $ elAttr "div" ("style" =: ("width:" <> showT (round (f * 100) :: Int) <> "%")) blank
  elClass "span" "val" (text val)

machineCard :: W t m => Dynamic t UTCTime -> MachineNow -> m ()
machineCard clock m = elClass "div" "card" $ do
  elClass "div" "title" $ do
    elClass "span" "name" $ elAttr "a" ("href" =: ("#/machine/" <> mName m)) (text (mName m))
    elClass "span" "sub" $ text $ T.intercalate " · " $ filter (not . T.null)
      [ maybe "" (\c -> showT c <> " cores") (mCpuCount m)
      , maybe "" (fmtBytes . fromIntegral) (mMemTotal m)
      , if mDisabled m then "DISABLED" else ""
      , if isJust (mLastSample m) then "" else "no agent" ]
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
    let others = M.toList (M.filter (>= 0.05) (mClasses m))
    when (not (null others)) $ elClass "div" "muted" $ text $
      "Other: " <> T.intercalate ", " [ k <> " " <> fmtNum v <> " cores" | (k, v) <- others ]
  elClass "div" "slots" $ do
    forM_ (mSlots m) $ \s -> elClass "div" "slot" $ do
      elAttr "div" ("class" =: "drv" <> "title" =: fromMaybe (slDrvName s) (slDrvPath s)) $ do
        forM_ (slKind s) $ \k -> elClass "span" ("tag " <> (if isJust (slBuild s) then k else "ifd")) (text (if isJust (slBuild s) then k else "not a hydra step")) >> text " "
        case slBuild s of
          Just b -> elAttr "a" ("href" =: ("#/build/" <> showT b)) (text (slDrvName s))
          Nothing -> text (slDrvName s)
      elClass "div" "meta" $ do
        forM_ (slStarted s) $ \st -> el "span" $ dynText (ffor clock (`fmtAgo` st))
        forM_ (slCores s) $ \c -> el "span" (text (fmtNum c <> " cores"))
        forM_ (slRss s) $ \r -> el "span" (text (fmtBytes (fromIntegral r)))
        forM_ (slNetRxRate s) $ \r -> when (r > 1e5) $ el "span" (text ("↓" <> fmtRate r))
        forM_ (slJobset s) $ \j -> el "span" (text j)
      forM_ (slJob s) $ \j -> elAttr "div" ("class" =: "meta mono" <> "title" =: j) (text (T.takeEnd 90 j))
    let free = fromMaybe 0 (mMaxJobs m) - length [ () | s <- mSlots m, isJust (slBuild s) ]
    forM_ [1 .. free] $ \_ -> elClass "div" "slot free" (text "free slot")

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
  el "h2" (text "What uses the farm")
  elClass "p" "muted" (text "Finished build steps grouped by a property of the derivation or job. CPU and memory are measured by the agents (steps on machines without an agent count only towards wall time).")
  (groupD, metricD) <- elClass "div" "controls" $ do
    text "Group by"
    g <- dropdown "package" (constDyn (M.fromList [ (k, k) | k <- ["package", "component_kind", "component", "platform", "job_compiler", "project", "jobset", "job", "drv_name", "system", "machine", "run_kind"] ])) def
    text "Order by"
    mt <- dropdown "cpu" (constDyn (M.fromList [("cpu", "CPU hours"), ("wall", "wall hours"), ("rss", "peak memory"), ("net", "network in"), ("steps", "steps"), ("import", "import hours")])) def
    pure (value g, value mt)
  hoursD <- rangeSelector 24
  let url = (\g mt h -> "api/top?group=" <> g <> "&metric=" <> mt <> "&limit=40&" <> rangeQuery h) <$> groupD <*> metricD <*> hoursD
  tD <- fetchDyn Nothing url :: m (Dynamic t (Maybe Table))
  let barsOf mt (Table cols rows) =
        let col = case mt of
              "wall" -> "wall_h"; "rss" -> "max_peak_rss"; "net" -> "net_rx_gb"; "steps" -> "steps"; "import" -> "import_h"; _ -> "cpu_h"
            idx = length (takeWhile (/= col) cols)
        in [ (case r of (k : _) -> k; [] -> A.Null, case drop idx r of (v : _) -> v; [] -> A.Null) | r <- take 25 rows ]
  chartWidget "chart tall" "bars" $ (\mt -> fmap (\t -> (barsOf mt t, A.object ["unit" .= (if mt == "rss" then "bytes" else "" :: Text)]))) <$> metricD <*> tD
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

