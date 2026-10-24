-- | Just enough of a .drv file: its input derivations, system and required
-- system features.  The server runs on the hydra host, so the derivations
-- hydra is building are in its store.
module Hydrometer.Server.Drv
  ( DrvInfo (..)
  , readDrv
  , parseDrv
  , storePath
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T

data DrvInfo = DrvInfo
  { dInputs   :: ![Text]
    -- ^ Full store paths of the input derivations.
  , dSystem   :: !Text
  , dFeatures :: ![Text]
    -- ^ requiredSystemFeatures.
  } deriving stock (Show, Eq)

-- | The full path of a store object given by its basename or full path.
storePath :: Text -> Text
storePath p = if "/" `T.isPrefixOf` p then p else "/nix/store/" <> p

readDrv :: Text -> IO (Maybe DrvInfo)
readDrv p = do
  r <- try (BS.readFile (T.unpack (storePath p))) :: IO (Either SomeException ByteString)
  pure (either (const Nothing) parseDrv r)

-- | The ATerm terms a .drv uses.
data Term = Str !Text | List [Term] | Tuple [Term]

-- | @Derive(outputs, inputDrvs, inputSrcs, system, builder, args, env)@,
-- or the same inside @DrvWithVersion("...", ...)@.
parseDrv :: ByteString -> Maybe DrvInfo
parseDrv bs0 = do
  rest <- BS.stripPrefix "Derive" bs0 <|> BS.stripPrefix "DrvWithVersion" bs0
  (t, _) <- term rest
  fields <- case t of
    Tuple [Str _version, a, b, c, d, e, f, g] -> Just [a, b, c, d, e, f, g]
    Tuple fs -> Just fs
    _ -> Nothing
  case fields of
    [_outputs, List inputDrvs, _srcs, Str system, _builder, _args, List env] ->
      let envPairs = [ (k, v) | Tuple [Str k, Str v] <- env ]
      in Just DrvInfo
        { dInputs = [ p | Tuple (Str p : _) <- inputDrvs ]
        , dSystem = system
        , dFeatures = features envPairs }
    _ -> Nothing
  where
    features envPairs = case lookup "__json" envPairs of
      Just j | Just (A.Object o) <- A.decode (BL.fromStrict (T.encodeUtf8 j))
             , Just (A.Array fs) <- KM.lookup "requiredSystemFeatures" o ->
               [ f | A.String f <- foldr (:) [] fs ]
      _ -> maybe [] T.words (lookup "requiredSystemFeatures" envPairs)

-- | Parse one term, returning the rest of the input.
term :: ByteString -> Maybe (Term, ByteString)
term s = case BC.uncons s of
  Just ('"', r) -> str r
  Just ('[', r) -> seqOf ']' List r
  Just ('(', r) -> seqOf ')' Tuple r
  _ -> Nothing
  where
    seqOf close mk r = case BC.uncons r of
      Just (c, r') | c == close -> Just (mk [], r')
      _ -> go [] r
      where
        go acc x = do
          (t, x') <- term x
          case BC.uncons x' of
            Just (',', x'') -> go (t : acc) x''
            Just (c, x'') | c == close -> Just (mk (reverse (t : acc)), x'')
            _ -> Nothing
    -- A string with @\\@ escapes for quote, backslash, n, r and t.
    str r = go [] r
      where
        go acc x =
          let (chunk, more) = BC.break (\c -> c == '"' || c == '\\') x
              acc' = chunk : acc
          in case BC.uncons more of
            Just ('"', after) -> Just (Str (decode (BS.concat (reverse acc'))), after)
            Just ('\\', esc) -> case BC.uncons esc of
              Just (e, after) -> go (BC.singleton (unescape e) : acc') after
              Nothing -> Nothing
            _ -> Nothing
    unescape = \case 'n' -> '\n'; 'r' -> '\r'; 't' -> '\t'; c -> c
    decode = T.decodeUtf8With T.lenientDecode
