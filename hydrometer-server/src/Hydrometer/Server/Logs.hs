-- | Errors and warnings in hydra's journal.  Every minute, read the new
-- entries of the configured systemd units that mention an error or warning,
-- group them by message pattern (store paths, hashes, ids and numbers
-- replaced) and count them per minute in hydrometer.log_counts.
module Hydrometer.Server.Logs
  ( runLogScanner
  , journalPatterns
  , recentLogPatterns
  , classify
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forever, unless, void)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isAlphaNum, isDigit, isLower)
import Data.IORef
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStrLn, stderr)
import System.Process

import Hydrometer.Api (LogPattern (..))
import Hydrometer.Server.Db

runLogScanner :: Db -> [Text] -> IO ()
runLogScanner d units = do
  cursor <- newIORef Nothing
  forever $ do
    r <- try (scan cursor) :: IO (Either SomeException ())
    either (\e -> hPutStrLn stderr ("log scanner: " <> displayException e)) pure r
    threadDelay (60 * 1000000)
  where
    scan cursor = do
      c <- readIORef cursor
      (code, out) <- readJournal (journalArgs units (maybe ["--since=-15min"] (\x -> ["--after-cursor=" <> x]) c))
      unless (code /= ExitSuccess && BS.null out) $ do
        let entries = [ e | l <- BC.lines out, Just e <- [entry l] ]
            counts = M.fromListWith (\(n1, _) (n2, ex) -> (n1 + n2, ex))
              [ ((minute t, u, lvl, pat), (1 :: Int, ex)) | (t, u, lvl, pat, ex, _) <- entries ]
        unless (M.null counts) $ withConn d $ \conn -> void $ executeMany conn
          "INSERT INTO hydrometer.log_counts VALUES (?,?,?,?,?,?) \
          \ON CONFLICT (t, unit, level, pattern) DO UPDATE SET n = log_counts.n + EXCLUDED.n, example = EXCLUDED.example"
          [ (t, u, lvl, pat, n, ex) | ((t, u, lvl, pat), (n, ex)) <- M.toList counts ]
        case [ cur | (_, _, _, _, _, cur) <- entries ] of
          [] -> pure ()
          curs -> writeIORef cursor (Just (T.unpack (last curs)))
    -- Counts are kept per minute.
    minute :: UTCTime -> UTCTime
    minute t = posixSecondsToUTCTime (fromIntegral ((floor (utcTimeToPOSIXSeconds t) :: Int64) `div` 60 * 60))

journalArgs :: [Text] -> [String] -> [String]
journalArgs units extra =
  ["--no-pager", "--output=json", "--grep=(?i)(error|warn|fail)"]
  ++ concat [ ["--unit", T.unpack u] | u <- units ] ++ extra

-- | journalctl exits 1 when --grep matches nothing.
readJournal :: [String] -> IO (ExitCode, BS.ByteString)
readJournal args = do
  (_, Just hout, Just herr, ph) <- createProcess (proc "journalctl" args)
    { std_out = CreatePipe, std_err = CreatePipe }
  out <- BS.hGetContents hout
  _ <- BS.hGetContents herr
  hClose hout
  code <- waitForProcess ph
  pure (code, out)

-- | The patterns in the units' journal since @since@ (a journalctl --since
-- value), most frequent first: (unit, level, pattern, count, example).
journalPatterns :: [Text] -> String -> IO [(Text, Text, Text, Int, Text)]
journalPatterns units since = do
  (_, out) <- readJournal (journalArgs units ["--since=" <> since])
  let counts = M.fromListWith (\(n1, _) (n2, ex) -> (n1 + n2, ex))
        [ ((u, lvl, pat), (1 :: Int, ex)) | l <- BC.lines out, Just (_, u, lvl, pat, ex, _) <- [entry l] ]
  pure [ (u, lvl, pat, n, ex) | ((u, lvl, pat), (n, ex)) <- sortOn (Down . fst . snd) (M.toList counts) ]

-- | One journal entry: (time, unit, level, pattern, example, cursor).
entry :: BS.ByteString -> Maybe (UTCTime, Text, Text, Text, Text, Text)
entry l = do
  A.Object o <- A.decodeStrict l
  msg <- message =<< KM.lookup "MESSAGE" o
  unit <- str =<< (KM.lookup "_SYSTEMD_UNIT" o)
  cur <- str =<< KM.lookup "__CURSOR" o
  us <- str =<< KM.lookup "__REALTIME_TIMESTAMP" o
  t <- case reads (T.unpack us) :: [(Integer, String)] of
    [(n, "")] -> Just (posixSecondsToUTCTime (fromIntegral n / 1000000))
    _ -> Nothing
  (lvl, pat) <- classify (prio =<< KM.lookup "PRIORITY" o) msg
  pure (t, fromMaybe unit (T.stripSuffix ".service" unit), lvl, pat, T.take 400 (clean msg), cur)
  where
    str = \case A.String s -> Just s; _ -> Nothing
    -- journald gives a non-UTF-8 message as an array of bytes.
    message = \case
      A.String s -> Just s
      A.Array bs -> Just (T.decodeUtf8With T.lenientDecode (BS.pack [ fromIntegral n | A.Number n0 <- foldr (:) [] bs, let n = truncate n0 :: Int ]))
      _ -> Nothing
    prio v = str v >>= \p -> case reads (T.unpack p) of [(n, "")] -> Just (n :: Int); _ -> Nothing

-- | Strip terminal colour codes.
clean :: Text -> Text
clean t = case T.breakOn "\ESC[" t of
  (a, b) | T.null b -> a
         | otherwise -> a <> clean (T.drop 1 (T.dropWhile (\c -> c /= 'm') b))

-- | The level and pattern of a message, if it is an error or a warning.
-- Rust tracing logs at syslog priority 6 whatever its level, so the level
-- comes from its level token; plain lines (Perl, nix, the attic bridge) go by
-- keywords.
classify :: Maybe Int -> Text -> Maybe (Text, Text)
classify priority raw = do
  lvl <- case (priority, tracingLevel) of
    (Just p, _) | p <= 3 -> Just "error"
                | p == 4 -> Just "warning"
    (_, Just "ERROR") -> Just "error"
    (_, Just "WARN") -> Just "warning"
    (_, Just _) -> Nothing
    -- Not a bare "failed": "cleared 0 failed paths" is routine.
    _ | has ["ERROR:", "error:", "Error:", " failed with", " failed to", "Failed to", " failed:", " failed after"] -> Just "error"
      | has ["WARNING:", "warning:", "Warning:"] -> Just "warning"
      | otherwise -> Nothing
  pure (lvl, T.take 200 (normalise (dropFields (dropPrefix msg))))
  where
    msg = T.strip (clean raw)
    has = any (`T.isInfixOf` msg)
    tracingLevel = case filter (`elem` ["ERROR", "WARN", "INFO", "DEBUG", "TRACE"]) (take 2 (T.words msg)) of
      (l : _) -> Just l
      [] -> Nothing
    -- Tracing: "2026-10-23T23:18:49.023682Z ERROR span:span: crate::module: text"
    -- -> "crate::module: text".
    dropPrefix m =
      let ws = T.words m
          ws1 = case ws of (w : r) | "Z" `T.isSuffixOf` w, T.any isDigit w -> r; _ -> ws
          ws2 = case ws1 of (w : r) | w `elem` ["ERROR", "WARN", "INFO", "DEBUG"] -> r; _ -> ws1
          ws3 = case break (\w -> "::" `T.isInfixOf` w && ":" `T.isSuffixOf` w) ws2 of
                  (_, target : r) -> target : r
                  _ -> ws2
      in T.unwords ws3
    -- Trailing structured fields: "... method=POST uri=http://…".
    dropFields m = T.unwords (reverse (dropWhile isField (reverse (T.words m))))
    isField w = case T.breakOn "=" w of
      (k, v) -> not (T.null v) && not (T.null k) && T.all (\c -> isLower c || isDigit c || c == '_' || c == '.') k

-- | Replace the parts of a message that differ between occurrences.
normalise :: Text -> Text
normalise = runs . paths
  where
    paths t = case T.breakOn "/nix/store/" t of
      (a, b) | T.null b -> a
             | otherwise -> a <> "<path>" <> paths (T.dropWhile (`notElem` (" \t'\",;)]>" :: String)) b)
    runs t
      | T.null t = t
      | otherwise =
          let (word, rest) = T.span isAlphaNum t
              (other, rest') = T.span (not . isAlphaNum) rest
          in norm word <> other <> runs rest'
    norm w
      | T.null w = w
      | T.all isDigit w = "N"
      | T.length w >= 12 && T.any isDigit w = "<id>"
      | otherwise = w

-- | The most frequent patterns of the last 15 minutes.
recentLogPatterns :: Connection -> IO [LogPattern]
recentLogPatterns c = do
  rows <- query_ c
    "SELECT unit, level, pattern, sum(n)::int, (array_agg(example ORDER BY t DESC))[1], \
    \  (extract(epoch FROM max(t)) * 1000)::bigint \
    \FROM hydrometer.log_counts WHERE t > now() - interval '15 minutes' \
    \GROUP BY 1, 2, 3 ORDER BY 4 DESC LIMIT 12"
  pure [ LogPattern u l p n ex lst | (u, l, p, n, ex, lst) <- rows ]
