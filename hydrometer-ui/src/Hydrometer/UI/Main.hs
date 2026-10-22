{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Entry point and hash routing (@#/now@, @#/machine/NAME@, @#/jobs@,
-- @#/bottlenecks@, @#/evals@, @#/eval/ID@, @#/build/ID@).
--
-- Compiled with the GHC JavaScript backend this runs in the page.  Built
-- natively it is a jsaddle-warp development server: open
-- http://localhost:3912/ and set HYDROMETER_API to a running hydrometer
-- server (it serves the scripts and styles too).
module Hydrometer.UI.Main
  ( main
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import GHCJS.DOM.Types (liftJSM)
import Language.Javascript.JSaddle (eval, fun, jsg, valToText, (!), (#))
import Language.Javascript.JSaddle.Warp qualified as JW
import Reflex.Dom.Core
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Read (readMaybe)

import Hydrometer.UI.Common
import Hydrometer.UI.Pages

data Route
  = RNow | RMachine Text | RJobs | RBottlenecks | REvals | REval Int | RBuild Int
  deriving (Eq, Show)

parseRoute :: Text -> Route
parseRoute h = case filter (not . T.null) (T.splitOn "/" (T.dropWhile (`elem` ['#', '/']) h)) of
  ["machine", m] -> RMachine m
  ["jobs"] -> RJobs
  ["bottlenecks"] -> RBottlenecks
  ["evals"] -> REvals
  ["eval", i] | Just n <- readMaybe (T.unpack i) -> REval n
  ["build", i] | Just n <- readMaybe (T.unpack i) -> RBuild n
  _ -> RNow

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  api <- lookupEnv "HYDROMETER_API"
  JW.run 3912 $ do
    -- Native development: pull the page assets from the server.
    case api of
      Just base -> void $ eval (T.pack (devLoader base))
      Nothing -> pure ()
    mainWidgetInElementById "app" app
  where
    devLoader base = concat
      [ "window.hydrometerApi = '", base, "/';"
      , "if (!document.getElementById('app')) { var d = document.createElement('div'); d.id = 'app'; document.body.appendChild(d); }"
      , "['vendor/echarts.min.js', 'charts.js'].forEach(function (s) { var e = document.createElement('script'); e.src = '", base, "/' + s; e.async = false; document.head.appendChild(e); });"
      , "var l = document.createElement('link'); l.rel = 'stylesheet'; l.href = '", base, "/style.css'; document.head.appendChild(l);"
      ]

app :: forall t m. W t m => m ()
app = do
  routeD <- hashRoute
  elClass "header" "top" $ do
    elClass "span" "brand" (text "hydrometer")
    el "nav" $ mapM_ (navLink routeD)
      [ ("#/now", "Now", (== RNow)), ("#/jobs", "Jobs", (== RJobs))
      , ("#/bottlenecks", "Bottlenecks", (== RBottlenecks))
      , ("#/evals", "Evaluations", \case REvals -> True; REval _ -> True; _ -> False) ]
    elClass "span" "clock" $ elAttr "a" ("href" =: "api") (text "JSON API")
  routeU <- holdUniqDyn routeD
  el "main" $ dyn_ $ ffor routeU $ \case
    RNow -> nowPage
    RMachine m -> machinePage m
    RJobs -> jobsPage
    RBottlenecks -> bottlenecksPage
    REvals -> evalsPage
    REval i -> evalPage i
    RBuild b -> buildPage b
  where
    navLink routeD (href, label, active) =
      elDynAttr "a" ((\r -> "href" =: href <> "class" =: (if active r then "active" else "")) <$> routeD) (text label)

hashRoute :: W t m => m (Dynamic t Route)
hashRoute = do
  (ev, trigger) <- newTriggerEvent
  initial <- liftJSM currentHash
  liftJSM $ do
    w <- jsg ("window" :: Text)
    void $ w # ("addEventListener" :: Text) $ ("hashchange" :: Text, fun $ \_ _ _ -> currentHash >>= liftIO . trigger)
  holdDyn (parseRoute initial) (parseRoute <$> ev)
  where
    currentHash = valToText =<< jsg ("location" :: Text) ! ("hash" :: Text)
