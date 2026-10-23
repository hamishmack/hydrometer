{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | Shared widgets: fetching, formatting, tables and charts.
module Hydrometer.UI.Common
  ( W
  , apiUrl
  , fetchDyn
  , clockDyn
  , fmtBytes
  , fmtRate
  , fmtPct
  , fmtDuration
  , fmtNum
  , fmtFixed
  , fmtTime
  , fmtAgo
  , shortDrv
  , tableWidget
  , chartWidget
  , rangeSelector
  , rangeQuery
  , stat
  , statSub
  , loading
  ) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing, Down (..))
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHCJS.DOM.Types (MonadJSM, liftJSM)
import Language.Javascript.JSaddle (eval, JSNull (..), jsg, (#), valToText)
import Numeric (showFFloat)
import Reflex.Dom.Core

import Hydrometer.Api (Table (..))

-- | Everything the widgets need from the reflex-dom stack.
type W t m =
  ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m
  , PerformEvent t m, TriggerEvent t m, MonadIO m
  , MonadJSM m, MonadJSM (Performable m), MonadIO (Performable m)
  , DomBuilderSpace m ~ GhcjsDomSpace )

-- | The API lives next to the page (relative URLs work under any prefix);
-- the native development runner sets @window.hydrometerApi@ to a server.
apiUrl :: MonadJSM m => Text -> m Text
apiUrl path = liftJSM $ do
  -- `|| ''`: on the JS backend valToText of undefined throws.
  base <- valToText =<< eval ("window.hydrometerApi || ''" :: Text)
  pure (if T.null base then path else base <> path)

-- | Fetch and decode JSON whenever the URL changes, and every @period@
-- seconds if given.  Keeps the last good value.
fetchDyn :: forall t m a. (W t m, FromJSON a) => Maybe NominalDiffTime -> Dynamic t Text -> m (Dynamic t (Maybe a))
fetchDyn period urlD = do
  pb <- getPostBuild
  ticks <- case period of
    Nothing -> pure never
    Just p -> void <$> tickLossyFromPostBuildTime p
  let reqs = leftmost [updated urlD, tag (current urlD) pb, tag (current urlD) ticks]
  absReqs <- performEvent (apiUrl <$> reqs)
  performEvent_ (ffor absReqs $ \u -> liftIO (putStrLn ("hydrometer: GET " <> T.unpack u)))
  resp <- performRequestAsyncWithError (ffor absReqs $ \u -> xhrRequest "GET" u def)
  let decoded = ffor resp $ \case
        Left e -> Left ("request failed: " <> show e)
        Right r -> case _xhrResponse_responseText r of
          Nothing -> Left "no response"
          Just body -> A.eitherDecodeStrict (T.encodeUtf8 body)
  -- Decode failures go to the console: a silent "Loading…" is no help.
  performEvent_ (ffor (fmapMaybe (either Just (const Nothing)) decoded) $ \e ->
    liftIO (putStrLn ("hydrometer: bad API response: " <> e)))
  holdDyn Nothing (fmapMaybe (either (const Nothing) Just) decoded)

clockDyn :: W t m => m (Dynamic t UTCTime)
clockDyn = do
  t0 <- liftIO getCurrentTime
  ticks <- tickLossyFromPostBuildTime 1
  holdDyn t0 (_tickInfo_lastUTC <$> ticks)

fmtBytes :: Double -> Text
fmtBytes = go ["B", "KiB", "MiB", "GiB", "TiB"]
  where
    go (u : us) v | abs v >= 1024 && not (null us) = go us (v / 1024)
                  | otherwise = T.pack (showFFloat (Just (if u == "B" then 0 else 1)) v "") <> " " <> u
    go [] v = T.pack (show v)

fmtRate :: Double -> Text
fmtRate v = fmtBytes v <> "/s"

fmtPct :: Double -> Text
fmtPct v = T.pack (show (round (v * 100) :: Int)) <> "%"

fmtNum :: Double -> Text
fmtNum v
  | v == fromIntegral (round v :: Int) = T.pack (show (round v :: Int))
  | abs v >= 100 = T.pack (show (round v :: Int))
  | otherwise = T.pack (showFFloat (Just 2) v "")

-- | A fixed number of decimals, so a live value keeps its width.
fmtFixed :: Int -> Double -> Text
fmtFixed d v = T.pack (showFFloat (Just d) v "")

fmtDuration :: Double -> Text
fmtDuration s
  | s < 60 = T.pack (show (round s :: Int)) <> "s"
  | s < 3600 = T.pack (show (floor (s / 60) :: Int)) <> "m" <> pad (round s `mod` 60) <> "s"
  | otherwise = T.pack (show (floor (s / 3600) :: Int)) <> "h" <> pad ((round s `div` 60) `mod` 60) <> "m"
  where pad n = T.justifyRight 2 '0' (T.pack (show (n :: Int)))

fmtTime :: Int64 -> Text
fmtTime ms = T.pack (formatTime defaultTimeLocale "%m-%d %H:%M" (posixSecondsToUTCTime (fromIntegral ms / 1000)))

fmtAgo :: UTCTime -> Int64 -> Text
fmtAgo t ms = fmtDuration (realToFrac (utcTimeToPOSIXSeconds t) - fromIntegral ms / 1000)

-- | Drop the store hash and .drv suffix.
shortDrv :: Text -> Text
shortDrv t =
  let base = T.takeWhileEnd (/= '/') t
      noHash = if T.length base > 33 && T.index base 32 == '-' then T.drop 33 base else base
  in fromMaybe noHash (T.stripSuffix ".drv" noHash)

loading :: DomBuilder t m => m ()
loading = elClass "div" "loading" (text "Loading…")

stat :: DomBuilder t m => Text -> Text -> m ()
stat k v = elClass "div" "stat" $ do
  elClass "div" "k" (text k)
  elClass "div" "v" (text v)

-- | A stat with a small line under the value.  The line is always there
-- (empty or not) so tiles in a row keep the same height.
statSub :: DomBuilder t m => Text -> Text -> Text -> m ()
statSub k v s = elAttr "div" ("class" =: "stat" <> "title" =: (k <> ": " <> v <> (if T.null s then "" else ", " <> s))) $ do
  elClass "div" "k" (text k)
  elClass "div" "v" (text v)
  elClass "div" "s" (text s)

-- | How a table column is rendered, from its name.
data Fmt = FMono | FNum | FBytes | FSecs | FTime | FPct | FLink Text

columnFmt :: Text -> Fmt
columnFmt c
  | c `elem` ["build"] = FLink "#/build/"
  | c `elem` ["eval_id"] = FLink "#/eval/"
  | c `elem` ["machine"] = FLink "#/machine/"
  | c `elem` ["started", "stopped", "evaluated", "last_finish", "first_step"] = FTime
  | c `elem` ["drv_name", "job", "key", "component"] = FMono
  | "rss" `T.isInfixOf` c || "bytes" `T.isInfixOf` c = FBytes
  | c `elem` ["wall_s", "import_s", "build_s", "upload_s", "cpu_s", "eval_s"] = FSecs
  | "util" `T.isSuffixOf` c = FPct
  | otherwise = FNum

cell :: DomBuilder t m => Text -> A.Value -> m ()
cell col v = case (columnFmt col, v) of
  (_, A.Null) -> el "td" (text "")
  (FLink prefix, _) -> el "td" $ elAttr "a" ("href" =: (prefix <> plain v)) (text (plain v))
  (FTime, A.Number n) -> elClass "td" "num" (text (fmtTime (round (toRealFloat n :: Double))))
  (FBytes, A.Number n) -> elClass "td" "num" (text (fmtBytes (toRealFloat n)))
  (FSecs, A.Number n) -> elClass "td" "num" (text (fmtDuration (toRealFloat n)))
  (FPct, A.Number n) -> elClass "td" "num" (text (fmtPct (toRealFloat n)))
  (_, A.Number n) -> elClass "td" "num" (text (fmtNum (toRealFloat n)))
  (FMono, _) -> elAttr "td" ("class" =: "mono" <> "title" =: plain v) (text (plain v))
  _ -> el "td" (text (plain v))

plain :: A.Value -> Text
plain = \case
  A.String s -> s
  A.Number n -> fmtNum (toRealFloat n)
  A.Bool b -> bool "false" "true" b
  A.Null -> ""
  other -> T.decodeUtf8 (BL.toStrict (A.encode other))

-- | A sortable table.  Click a header to sort by it (again to reverse).
tableWidget :: forall t m. W t m => [Text] -> Dynamic t (Maybe Table) -> m ()
tableWidget hidden tD = elClass "div" "scroll" $ dyn_ $ ffor tD $ \case
  Nothing -> loading
  Just (Table cols rows) | null rows -> elClass "div" "muted" (text "No rows.")
                         | otherwise -> mdo
    let shown = [ (i, c) | (i, c) <- zip [0 :: Int ..] cols, c `notElem` hidden ]
    sortD <- foldDyn (\i (j, desc) -> if i == j then (j, not desc) else (i, True)) (-1, True) clicks
    clicks <- elClass "table" "data" $ do
      cs <- el "thead" $ el "tr" $ fmap leftmost $ mapM (\(i, c) -> do
              (e, _) <- el' "th" (text (T.replace "_" " " c))
              pure (i <$ domEvent Click e)) shown
      el "tbody" $ dyn_ $ ffor sortD $ \(si, desc) -> do
        let key r = case drop si r of (x : _) -> sortKey x; [] -> SNull
            sorted = if si < 0 then rows else sortBy (if desc then comparing (Down . key) else comparing key) rows
        mapM_ (\r -> el "tr" $ mapM_ (\(i, c) -> cell c (fromMaybe A.Null (lookupIx i r))) shown) sorted
      pure cs
    pure ()
  where
    lookupIx i r = case drop i r of (x : _) -> Just x; [] -> Nothing

data SortKey = SNull | SNum Double | SText Text deriving (Eq, Ord)

sortKey :: A.Value -> SortKey
sortKey = \case
  A.Number n -> SNum (toRealFloat n)
  A.String s -> SText s
  A.Null -> SNull
  v -> SText (plain v)

-- | A chart div driven by @hmCharts.<fn>(el, json, opts)@ in charts.js.
chartWidget :: forall t m a. (W t m, ToJSON a) => Text -> Text -> Dynamic t (Maybe (a, A.Value)) -> m ()
chartWidget cls fn argsD = do
  (e, _) <- elAttr' "div" ("class" =: cls) blank
  pb <- getPostBuild
  let raw = _element_raw e
      draws = fmapMaybe id (leftmost [updated argsD, tag (current argsD) pb])
  performEvent_ $ ffor draws $ \(dat, opts) -> liftJSM $ do
    let enc :: ToJSON x => x -> Text
        enc = T.decodeUtf8 . BL.toStrict . A.encode
    -- Defer a frame: on first build the div is not attached and has no size.
    void $ jsg ("hmDraw" :: Text) # ("call" :: Text) $ (JSNull, fn, raw, enc dat, enc opts)

-- | 1h / 6h / 24h / 3d / 7d buttons.  Returns hours.
rangeSelector :: W t m => Int -> m (Dynamic t Int)
rangeSelector initial = elClass "div" "controls" $ mdo
  text "Range:"
  hoursD <- holdDyn initial (leftmost evs)
  evs <- mapM (\(lbl, h) -> do
           (e, _) <- elDynAttr' "button" ((\cur -> "class" =: (if cur == h then "on" else "")) <$> hoursD) (text lbl)
           pure (h <$ domEvent Click e))
         [("1h", 1), ("6h", 6), ("24h", 24), ("3d", 72), ("7d", 168), ("30d", 720)]
  pure hoursD

-- | @from@ relative to now (the server reads a negative from as "ms ago").
rangeQuery :: Int -> Text
rangeQuery hours = "from=-" <> T.pack (show (hours * 3600000))

