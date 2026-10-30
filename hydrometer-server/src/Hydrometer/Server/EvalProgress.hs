-- | How far a running evaluation has got.  Nothing hydra-eval-jobset does
-- reaches the database until its transaction commits at the end, so this
-- reads the progress lines it logs to hydra-evaluator's journal:
--
-- > (project:jobset) Fetching input `src` (git) ...
-- > (project:jobset) Evaluating...
-- > (project:jobset) evaluated 6120 jobs (3 failed)
-- > (project:jobset) evaluated 10054 jobs (3 failed), adding builds
-- > (project:jobset) adding builds: checked 4000 of 10054 jobs
--
-- (the last three from hamishmack/hydra's hkm/eval-progress; without them
-- the phase is all there is).
module Hydrometer.Server.EvalProgress
  ( EvalProgress (..)
  , evalProgress
  , parseProgress
  ) where

import Control.Exception (SomeException, try)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Process (readProcessWithExitCode)

data EvalProgress = EvalProgress
  { epPhase   :: !Text
    -- ^ @fetching@, @evaluating@ or @adding@.
  , epJobs    :: !(Maybe Int)
    -- ^ Jobs nix-eval-jobs has returned so far (all of them once adding).
  , epFailed  :: !(Maybe Int)
  , epChecked :: !(Maybe Int)
    -- ^ While adding: jobs checked against existing builds.
  } deriving stock (Show, Eq)

-- | The latest progress line of the evaluation of @project:jobset@ that
-- started at @startedMs@.
evalProgress :: Text -> Int64 -> IO (Maybe EvalProgress)
evalProgress name startedMs = do
  r <- try (readProcessWithExitCode "journalctl"
    [ "--no-pager", "--unit=hydra-evaluator", "--since=@" <> show (startedMs `div` 1000)
    , "--reverse", "--lines=1", "--output=cat"
    , "--grep=^\\(\\Q" <> T.unpack name <> "\\E\\) (Fetching input|Evaluating|evaluated|adding builds)" ] "")
  pure $ case r of
    Right (_, out, _) -> parseProgress =<< listToMaybe (T.lines (T.pack out))
    Left (_ :: SomeException) -> Nothing

-- | One progress line.
parseProgress :: Text -> Maybe EvalProgress
parseProgress l = case T.words (T.drop 2 (T.dropWhile (/= ')') l)) of
  ("Fetching" : _) -> Just (EvalProgress "fetching" Nothing Nothing Nothing)
  -- No count yet: hydra without the progress lines never gives one.
  ("Evaluating..." : _) -> Just (EvalProgress "evaluating" Nothing Nothing Nothing)
  ["evaluated", n, "jobs", f, "failed)"] -> Just (EvalProgress "evaluating" (int n) (int (T.drop 1 f)) Nothing)
  ["evaluated", n, "jobs", f, "failed),", "adding", "builds"] ->
    Just (EvalProgress "adding" (int n) (int (T.drop 1 f)) (Just 0))
  ["adding", "builds:", "checked", i, "of", n, "jobs"] -> Just (EvalProgress "adding" (int n) Nothing (int i))
  _ -> Nothing
  where
    int t = case TR.decimal t of
      Right (v, "") -> Just v
      _ -> Nothing
