{-# LANGUAGE TemplateHaskell #-}
-- | Database access: a small connection pool, migrations, and a generic
-- "run this SQL and give me a 'Table'" helper used by the API, the CLI and
-- the MCP server.
module Hydrometer.Server.Db
  ( Db
  , openDb
  , withConn
  , withTx
  , migrate
  , queryTable
  , queryTableReadOnly
  , tableToSeries
  , epochMs
  , fromEpochMs
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (forM_, replicateM, unless, void, when)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.FileEmbed (embedFile)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal qualified as PSI
import Database.PostgreSQL.Simple.Types (Query (..))

import Hydrometer.Api (Series (..), Table (..))

data Db = Db
  { dbConnStr :: ByteString
  , dbPool    :: TQueue Connection
  }

openDb :: Text -> Int -> IO Db
openDb connStr n = do
  let cs = T.encodeUtf8 connStr
  conns <- replicateM n (connectPostgreSQL cs)
  q <- newTQueueIO
  atomically (mapM_ (writeTQueue q) conns)
  pure (Db cs q)

-- | Borrow a connection.  A connection that saw an exception is replaced,
-- since it may be mid-transaction or broken.
withConn :: Db -> (Connection -> IO a) -> IO a
withConn d act = mask $ \restore -> do
  c <- atomically (readTQueue (dbPool d))
  r <- try (restore (act c))
  case r of
    Right a -> atomically (writeTQueue (dbPool d) c) >> pure a
    Left (e :: SomeException) -> do
      _ <- try (close c) :: IO (Either SomeException ())
      c' <- try (connectPostgreSQL (dbConnStr d))
      case c' of
        Right nc -> atomically (writeTQueue (dbPool d) nc)
        -- Put the dead one back so the pool keeps its size; the next user
        -- gets an error and tries again.
        Left (_ :: SomeException) -> atomically (writeTQueue (dbPool d) c)
      throwIO e

withTx :: Db -> (Connection -> IO a) -> IO a
withTx d act = withConn d (\c -> withTransaction c (act c))

migrations :: [(Int, ByteString)]
migrations =
  [ (1, $(embedFile "sql/001_init.sql"))
  , (2, $(embedFile "sql/002_builders.sql"))
  ]

viewsSql :: ByteString
viewsSql = $(embedFile "sql/views.sql")

-- | Apply outstanding numbered migrations, then (re)create the views.
migrate :: Db -> IO ()
migrate d = withConn d $ \c -> do
  -- CREATE SCHEMA IF NOT EXISTS needs CREATE on the database even when the
  -- schema exists; deployments create it for us (see nix/nixos.nix).
  [Only haveSchema] <- query_ c "SELECT EXISTS (SELECT 1 FROM pg_namespace WHERE nspname = 'hydrometer')"
  unless haveSchema $ void $ execute_ c "CREATE SCHEMA hydrometer"
  void $ execute_ c "CREATE TABLE IF NOT EXISTS hydrometer.schema_version (version int PRIMARY KEY, applied timestamptz NOT NULL DEFAULT now())"
  applied <- map fromOnly <$> query_ c "SELECT version FROM hydrometer.schema_version" :: IO [Int]
  forM_ (sortOn fst migrations) $ \(v, sql) -> unless (v `elem` applied) $
    withTransaction c $ do
      execRaw c sql
      void $ execute c "INSERT INTO hydrometer.schema_version (version) VALUES (?)" (Only v)
  withTransaction c (execRaw c viewsSql)

-- | Run a multi-statement script (no parameters).
execRaw :: Connection -> ByteString -> IO ()
execRaw c sql = PSI.withConnection c $ \pq -> do
  mr <- PQ.exec pq sql
  case mr of
    Nothing -> fail "exec: no result"
    Just r -> do
      st <- PQ.resultStatus r
      when (st `notElem` [PQ.CommandOk, PQ.TuplesOk]) $ do
        msg <- PQ.resultErrorMessage r
        fail ("SQL failed: " <> maybe "" BC.unpack msg)

-- | Run a query (parameters interpolated with postgresql-simple's quoting)
-- and return its columns and rows as JSON values, in column order.
queryTable :: ToRow p => Connection -> Query -> p -> IO Table
queryTable c q p = do
  sql <- formatQuery c q p
  tableFromSql c sql

-- | Run arbitrary SQL read-only, with a time limit and row cap.
queryTableReadOnly :: Connection -> Int -> Int -> Text -> IO Table
queryTableReadOnly c timeoutMs maxRows sql = withTransaction c $ do
  void $ execute_ c "SET TRANSACTION READ ONLY"
  _ <- query c "SELECT set_config('statement_timeout', ?, true)" (Only (show timeoutMs)) :: IO [Only Text]
  let body = T.dropWhileEnd (\ch -> ch == ';' || ch == ' ' || ch == '\n') (T.strip sql)
  t <- tableFromSql c (T.encodeUtf8 ("SELECT * FROM (" <> body <> "\n) _q LIMIT " <> T.pack (show maxRows)))
  pure t

tableFromSql :: Connection -> ByteString -> IO Table
tableFromSql c sql = do
  -- Column names first (LIMIT 0 costs nothing), then the rows as JSON
  -- arrays so values keep their SQL types and order.
  cols <- PSI.withConnection c $ \pq -> do
    mr <- PQ.exec pq ("SELECT * FROM (" <> sql <> "\n) _q LIMIT 0")
    r <- maybe (fail "query: no result") pure mr
    st <- PQ.resultStatus r
    unless (st == PQ.TuplesOk) $ do
      msg <- PQ.resultErrorMessage r
      fail (maybe "query failed" BC.unpack msg)
    n <- PQ.nfields r
    mapM (\k -> maybe "?" (T.decodeUtf8 ) <$> PQ.fname r k) [0 .. n - 1]
  if null cols then pure (Table [] []) else do
    let quoted = T.intercalate ", " [ "_q." <> quoteIdent col | col <- cols ]
        wrapped = "SELECT coalesce(json_agg(json_build_array(" <> T.encodeUtf8 quoted
               <> ")), '[]')::text FROM (" <> sql <> "\n) _q"
    [Only js] <- query_ c (Query wrapped)
    case A.eitherDecodeStrict (T.encodeUtf8 js) of
      Right rows -> pure (Table cols rows)
      Left e -> fail ("query: bad JSON from postgres: " <> e)
  where
    quoteIdent i = "\"" <> T.replace "\"" "\"\"" i <> "\""

-- | Turn a table whose first column is a time (epoch ms or timestamp) and
-- whose other columns are numbers into a 'Series'.
tableToSeries :: Int64 -> Table -> Series
tableToSeries step (Table cols rows) = Series
  { seTime = [ toMs t | (t : _) <- rows ]
  , seStepMs = step
  , seSeries = M.fromList
      [ (col, [ num (fromMaybe A.Null (lookupIx k r)) | r <- rows ])
      | (k, col) <- zip [1 :: Int ..] (drop 1 cols) ]
  }
  where
    lookupIx k r = case drop k r of (x : _) -> Just x; [] -> Nothing
    num = \case
      A.Number n -> Just (toRealFloat n)
      A.Bool b -> Just (if b then 1 else 0)
      _ -> Nothing
    toMs = \case
      A.Number n -> round (toRealFloat n :: Double)
      _ -> 0

epochMs :: UTCTime -> Int64
epochMs t = round (utcTimeToPOSIXSeconds t * 1000)

fromEpochMs :: Int64 -> UTCTime
fromEpochMs ms = posixSecondsToUTCTime (fromIntegral ms / 1000)

