-- | Do finished outputs reach the binary cache?  Every 10 minutes: look up
-- a sample of outputs of steps that finished 30-120 minutes ago (time enough
-- to have been uploaded) on the cache, and read hydra-attic-bridge's queue.
module Hydrometer.Server.CacheHealth
  ( runCacheMonitor
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, forever, void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)
import System.IO (hPutStrLn, stderr)

import Hydrometer.Api (CacheNow (..))
import Hydrometer.Server.Db

runCacheMonitor :: Db -> Manager -> Text -> TVar (Maybe CacheNow) -> IO ()
runCacheMonitor d mgr url st = forever $ do
  r <- try once :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left e -> do
      hPutStrLn stderr ("cache monitor: " <> displayException e)
      now <- epochMs <$> getCurrentTime
      atomically $ modifyTVar' st $ \old -> Just $ case old of
        Just c -> c { cnError = Just (T.pack (displayException e)) }
        Nothing -> CacheNow url now 0 0 [] Nothing Nothing Nothing (Just (T.pack (displayException e)))
  threadDelay (600 * 1000000)
  where
    base = T.dropWhileEnd (== '/') url
    once = do
      paths <- withConn d $ \c -> query_ c
        "SELECT o.path FROM buildstepoutputs o \
        \JOIN buildsteps s ON s.build = o.build AND s.stepnr = o.stepnr \
        \WHERE s.status = 0 AND o.path IS NOT NULL \
        \  AND s.stoptime BETWEEN extract(epoch FROM now() - interval '2 hours')::int \
        \                     AND extract(epoch FROM now() - interval '30 minutes')::int \
        \ORDER BY random() LIMIT 20" :: IO [Only Text]
      found <- forM paths $ \(Only p) -> (,) p <$> onCache p
      -- The bridge's table belongs to hydra-tools and may not exist (or not
      -- be readable); the cache check stands on its own.
      q <- try (withConn d $ \c -> query_ c
             "SELECT count(*)::int, count(*) FILTER (WHERE tries > 0)::int, \
             \  count(*) FILTER (WHERE tries = 0 AND last < now() - interval '1 hour')::int \
             \FROM drvpathstoupload") :: IO (Either SomeException [(Int, Int, Int)])
      let queue = case q of Right [x] -> Just x; _ -> Nothing
          present = length (filter snd found)
      now <- getCurrentTime
      withConn d $ \c -> void $ execute c
        "INSERT INTO hydrometer.cache_samples VALUES (?,?,?,?,?,?) ON CONFLICT DO NOTHING"
        (now, length found, present, fst3 <$> queue, snd3 <$> queue, thd3 <$> queue)
      atomically $ writeTVar st $ Just CacheNow
        { cnUrl = url, cnChecked = epochMs now, cnSampled = length found, cnPresent = present
        , cnMissing = [ p | (p, False) <- found ]
        , cnQueued = fst3 <$> queue, cnRetrying = snd3 <$> queue, cnStaleNew = thd3 <$> queue
        , cnError = Nothing }
    -- A narinfo is named by the store path's hash part.
    onCache p = do
      let hash = T.take 32 (T.takeWhileEnd (/= '/') p)
      req <- parseRequest (T.unpack (base <> "/" <> hash <> ".narinfo"))
      r <- try (httpNoBody req { responseTimeout = responseTimeoutMicro 15000000 } mgr)
             :: IO (Either SomeException (Response ()))
      pure (either (const False) ((== 200) . statusCode . responseStatus) r)
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c
