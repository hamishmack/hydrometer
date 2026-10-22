-- | Grouping processes into attributable runs.
--
-- A __build run__ is rooted at the process nix-daemon starts as a build user
-- (@_nixbldN@ on darwin, @nixbldN@ or an auto-allocated uid on linux).  Its
-- members are the root's descendants.  A linux build on a Mac is a
-- @nix-linux-builder@ process whose VM runs in a separate
-- @com.apple.Virtualization.VirtualMachine@ XPC process, parented to launchd
-- but running as the same build user; it is attached to the run by uid (a
-- build user is held by one build at a time).
--
-- An __evaluation run__ is rooted at @hydra-eval-jobset@ and covers its
-- @nix-eval-jobs@ workers.
--
-- Everything else is put in a coarse 'processClass'.
module Hydrometer.Agent.Attribution
  ( RootInfo (..)
  , Grouping (..)
  , isVmProcess
  , isLinuxBuilder
  , isEvalRoot
  , groupProcs
  , processClass
  , identifyFromEnv
  , identifyFromBuildJson
  , identifyEval
  ) where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sort)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Hydrometer.Agent.Sys
import Hydrometer.Types (RunKind (..))

-- | What a run root turned out to be.
data RootInfo = RootInfo
  { riKind    :: !RunKind
  , riName    :: !Text
  , riOutputs :: ![Text]
  , riSystem  :: !(Maybe Text)
  } deriving stock (Show, Eq)

data Grouping = Grouping
  { gRoots   :: ![(Proc, RunKind)]
    -- ^ Run roots and the kind of run they start.
  , gMembers :: !(IntMap [Proc])
    -- ^ Root pid -> member processes (the root included).
  , gOther   :: ![Proc]
    -- ^ Processes in no run.
  }

isVmProcess :: Proc -> Bool
isVmProcess p = "com.apple.Virtualization.VirtualMachine" `T.isInfixOf` pPath p

isLinuxBuilder :: Proc -> Bool
isLinuxBuilder p = "nix-linux-builder" `T.isSuffixOf` pPath p
  || pComm p == "nix-linux-builder"

isNixDaemon :: Proc -> Bool
-- Since nix 2.x the daemon is a symlink to `nix`, so its comm and path say
-- `nix`.  A root-owned `nix` process is almost always the daemon.
isNixDaemon p = pComm p == "nix-daemon" || "/bin/nix-daemon" `T.isSuffixOf` pPath p
  || (pComm p == "nix" && pUid p == 0)

-- | hydra-eval-jobset is a perl script; on linux its comm is truncated to 15
-- characters and may carry the nix wrapper's leading dot.
isEvalRoot :: Proc -> Bool
isEvalRoot p = any (`T.isPrefixOf` T.dropWhile (== '.') (pComm p)) ["hydra-eval-jobs"]
  && not ("hydra-eval-jobs-" `T.isPrefixOf` pComm p)

-- | Group a process table.  @isBuildUid@ says whether a uid is a nix build
-- user.
groupProcs :: (Int -> Bool) -> [Proc] -> Grouping
groupProcs isBuildUid procs = Grouping
  { gRoots = roots
  , gMembers = members
  , gOther = [ p | p <- procs, not (pPid p `IS.member` claimed) ]
  }
  where
    byPid = IM.fromList [ (pPid p, p) | p <- procs ]
    children = IM.fromListWith (++) [ (pPpid p, [p]) | p <- procs ]
    parentOf p = IM.lookup (pPpid p) byPid

    isBuildRoot p =
      not (isVmProcess p)
      && pPpid p /= 1   -- launchd-spawned per-user agents (distnoted, contactsd)
      && pUid p /= 0
      && case parentOf p of
           Just par -> pUid par /= pUid p && (isBuildUid (pUid p) || isNixDaemon par)
           Nothing -> False
      && (isBuildUid (pUid p) || maybe False isNixDaemon (parentOf p))

    buildRoots = [ (p, if isLinuxBuilder p then VmBuild else NativeBuild) | p <- procs, isBuildRoot p ]
    -- An evaluation nested inside another (it never is) would be claimed twice;
    -- take outermost roots only.
    evalRoots = [ (p, Evaluation) | p <- procs, isEvalRoot p
                , not (any isEvalRoot (ancestors p)) ]
    roots = buildRoots ++ evalRoots

    ancestors p = go (5000 :: Int) (parentOf p)
      where go 0 _ = []
            go _ Nothing = []
            go n (Just a) = a : go (n - 1) (if pPid a <= 1 then Nothing else parentOf a)

    descendants p = p : concatMap descendants (IM.findWithDefault [] (pPid p) children)

    vmByUid = M.fromListWith (++) [ (pUid p, [p]) | p <- procs, isVmProcess p ]

    members = IM.fromList
      [ (pPid r, ms)
      | (r, k) <- roots
      , let ms = descendants r ++ (if k == VmBuild then M.findWithDefault [] (pUid r) vmByUid else [])
      ]
    claimed = IS.fromList (concatMap (map pPid) (IM.elems members))

-- | The role of a process outside any run.  Children of a classified process
-- inherit its class.
processClass :: Int -> IntMap Proc -> Proc -> Text
processClass selfPid byPid = go (8 :: Int)
  where
    go 0 _ = "other"
    go n p
      | pPid p == selfPid = "hydrometer"
      | Just c <- own p = c
      | pPpid p <= 1 = "other"
      | Just par <- IM.lookup (pPpid p) byPid = go (n - 1) par
      | otherwise = "other"
    own p
      | isNixDaemon p = Just "nix-daemon"
      | isVmProcess p = Just "vm-unattributed"
      | any (`T.isInfixOf` pPath p) ["hydra-builder", "hydra-queue-builder"]
        || pComm p `elem` ["hydra-builder", "hydra-queue-bui"] = Just "hydra-builder"
      | "hydra-queue-run" `T.isPrefixOf` pComm p || "hydra-queue-runner" `T.isInfixOf` pPath p = Just "hydra-queue-runner"
      | "peernix" `T.isInfixOf` pComm p = Just "peernix"
      | "postgres" `T.isPrefixOf` pComm p = Just "postgres"
      | "hydra" `T.isPrefixOf` T.dropWhile (== '.') (pComm p) = Just "hydra"
      | "starman" `T.isPrefixOf` pComm p = Just "hydra"
      | pComm p `elem` ["nix", "nix-store", "nix-eval-jobs", "nix-build", "nix-instantiate"] = Just "nix"
      | pComm p `elem` ["kernel_task", "WindowServer", "mds", "mds_stores", "mdworker_shared"] = Just "system"
      | otherwise = Nothing

-- | Identify a native build from its root's environment.  stdenv builders
-- carry @name@, @system@ and one variable per output named in @outputs@.
identifyFromEnv :: [(Text, Text)] -> Maybe RootInfo
identifyFromEnv env = do
  let look k = lookup k env
  name <- look "name"
  let outNames = maybe ["out"] T.words (look "outputs")
      outs = mapMaybe look outNames
  if null outs then Nothing else Just RootInfo
    { riKind = NativeBuild, riName = name, riOutputs = sort outs, riSystem = look "system" }

-- | Identify a VM build from nix's external-builder @build.json@.
identifyFromBuildJson :: A.Value -> Maybe RootInfo
identifyFromBuildJson = A.parseMaybe $ A.withObject "build.json" $ \o -> do
  outputs <- o .: "outputs" :: A.Parser (M.Map Text Text)
  env <- fromMaybe M.empty <$> (o .:? "env" :: A.Parser (Maybe (M.Map Text Text)))
  system <- o .:? "system"
  let name = fromMaybe (maybe "" storeName (listToMaybe (M.elems outputs))) (M.lookup "name" env)
  pure RootInfo { riKind = VmBuild, riName = name, riOutputs = sort (M.elems outputs), riSystem = system }
  where
    storeName = T.drop 1 . T.dropWhile (/= '-') . T.takeWhileEnd (/= '/')

-- | @hydra-eval-jobset <project> <jobset>@.
identifyEval :: [Text] -> Maybe RootInfo
identifyEval argv = case dropWhile (not . ("hydra-eval-jobset" `T.isSuffixOf`)) argv of
  (_ : project : jobset : _) -> Just RootInfo
    { riKind = Evaluation, riName = project <> ":" <> jobset, riOutputs = [], riSystem = Nothing }
  _ -> Nothing
