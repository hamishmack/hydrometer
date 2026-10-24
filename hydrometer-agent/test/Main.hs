module Main (main) where

import Control.Monad (unless)
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IntMap.Strict qualified as IM
import Data.IORef
import Data.Text (Text)
import System.Exit (exitFailure)

import Hydrometer.Agent.Attribution
import Hydrometer.Agent.Sys
import Hydrometer.Types

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: (Eq a, Show a) => String -> a -> a -> IO ()
      check name want got = unless (want == got) $ do
        putStrLn ("FAIL " <> name <> "\n  want: " <> show want <> "\n  got:  " <> show got)
        modifyIORef failures (+ 1)

  -- KERN_PROCARGS2: argc=2, exec path, padding, argv, env.
  let blob = BS.pack [2, 0, 0, 0] <> "/bin/bash\0\0\0" <> "bash\0-e\0" <> "out=/nix/store/abc-foo\0name=foo\0\0junk"
  check "procargs2" (Just (ProcArgs ["bash", "-e"] [("out", "/nix/store/abc-foo"), ("name", "foo")]))
        (parseProcArgs2 blob)

  check "identifyFromEnv multiple outputs"
    (Just (RootInfo NativeBuild "foo-1.0" ["/nix/store/a-foo-1.0", "/nix/store/b-foo-1.0-dev"] (Just "aarch64-darwin")))
    (identifyFromEnv [("name", "foo-1.0"), ("outputs", "out dev"), ("out", "/nix/store/a-foo-1.0")
                     , ("dev", "/nix/store/b-foo-1.0-dev"), ("system", "aarch64-darwin")])

  let bj = A.decodeStrict (BC.pack "{\"system\":\"x86_64-linux\",\"outputs\":{\"out\":\"/nix/store/xyz-hello-1.0\"},\"env\":{\"name\":\"hello-1.0\"}}")
  check "identifyFromBuildJson"
    (Just (RootInfo VmBuild "hello-1.0" ["/nix/store/xyz-hello-1.0"] (Just "x86_64-linux")))
    (bj >>= identifyFromBuildJson)

  check "identifyEval"
    (Just (RootInfo Evaluation "haskell-nix:pullrequest-2606" [] Nothing))
    (identifyEval ["perl", "/nix/store/x-hydra/bin/hydra-eval-jobset", "haskell-nix", "pullrequest-2606"])

  check "identifyEval wrapped"
    (Just (RootInfo Evaluation "ican-group-haskell-nix:master" [] Nothing))
    (identifyEval ["/nix/store/p-perl/bin/perl", "/nix/store/h-hydra/bin/.hydra-eval-jobset-wrapped", "ican-group-haskell-nix", "master"])

  -- A process table: launchd(1), nix-daemon(100, root) -> worker(101, root)
  -- -> nix-linux-builder(200, uid 310) ; VM(201, uid 310, ppid 1);
  -- worker -> bash(300, uid 311) -> clang(301, uid 311);
  -- distnoted(400, uid 311, ppid 1); hydra-builder(500).
  let p pid ppid uid comm path = Proc pid ppid uid 0 0 0 0 0 0 0 comm path
      procs =
        [ p 1 0 0 "launchd" "/sbin/launchd"
        , p 100 1 0 "nix-daemon" "/nix/store/x-nix/bin/nix-daemon"
        , p 101 100 0 "nix-daemon" "/nix/store/x-nix/bin/nix-daemon"
        , p 200 101 310 "nix-linux-builder" "/nix/store/y-nix-linux-builder-0.3.0/bin/nix-linux-builder"
        , p 201 1 310 "com.apple.Virtualization.VirtualMachine" "/System/Library/Frameworks/Virtualization.framework/Versions/A/XPCServices/com.apple.Virtualization.VirtualMachine.xpc/Contents/MacOS/com.apple.Virtualization.VirtualMachine"
        , p 300 101 311 "bash" "/nix/store/z-bash/bin/bash"
        , p 301 300 311 "clang" "/nix/store/c-clang/bin/clang"
        , p 400 1 311 "distnoted" "/usr/sbin/distnoted"
        , p 500 1 501 "hydra-builder" "/nix/store/h-hydra-builder-0.1/bin/hydra-builder"
        ]
      g = groupProcs (`elem` [310, 311]) procs
      byPid = IM.fromList [ (pPid q, q) | q <- procs ]
  check "roots" [(200, VmBuild), (300, NativeBuild)] [ (pPid r, k) | (r, k) <- gRoots g ]
  check "vm members" (Just [200, 201]) (map pPid <$> IM.lookup 200 (gMembers g))
  check "native members" (Just [300, 301]) (map pPid <$> IM.lookup 300 (gMembers g))
  check "others" [1, 100, 101, 400, 500] (map pPid (gOther g))
  check "classes" ["other", "nix-daemon", "nix-daemon", "other", "hydra-builder"]
    (map (processClass 0 byPid) (gOther g) :: [Text])

  check "parseBuilderToml"
    (Just 4, ["aarch64-linux", "x86_64-linux"])
    (parseBuilderToml "buildDirAvailThreshold = 10.0\nmandatoryFeatures = []\nmaxJobs = 4\nsupportedFeatures = [\"big-parallel\", \"benchmark\"]\nsystems = [\"aarch64-linux\", \"x86_64-linux\"]\nuseSubstitutes = true\n")
  check "parseBuilderToml without the keys" (Nothing, []) (parseBuilderToml "pingInterval = 10\n")

  n <- readIORef failures
  if n == 0 then putStrLn "all tests passed" else exitFailure
