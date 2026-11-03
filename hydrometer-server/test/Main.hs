module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Exit (exitFailure)

import Hydrometer.Server.EvalIfd

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: (Eq a, Show a) => String -> a -> a -> IO ()
      check name want got = unless (want == got) $ do
        putStrLn ("FAIL " <> name <> "\n  want: " <> show want <> "\n  got:  " <> show got)
        modifyIORef failures (+ 1)

  -- Lines from hydra-evaluator's journal on linux-0 (2026-11-03) and from a
  -- test evaluation with two workers sharing one IFD.
  check "IFD start"
    (Just (IfdStart 500424 ["/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv"] "«unknown»"))
    (parseTrace "IFD start pid=500424 n=1 drvs=/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv^out at «unknown»")
  check "IFD start, several drvs, a position"
    (Just (IfdStart 7 ["/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-a.drv", "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-b.drv"] "/nix/store/x-source/default.nix:12:3"))
    (parseTrace "IFD start pid=7 n=2 drvs=/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-a.drv^out,/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-b.drv^out,dev at /nix/store/x-source/default.nix:12:3")
  check "IFD done, built"
    (Just (IfdDone (Just 500423) "/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv" "built" 9409))
    (parseTrace "IFD #1: pid=500423 /nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv^out [built] 9409ms at «unknown»")
  check "IFD done, waited on another worker"
    (Just (IfdDone (Just 500424) "/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv" "already-valid" 17957))
    (parseTrace "IFD #1: pid=500424 /nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv^out [already-valid] 17957ms at «unknown»")
  check "IFD done, mis-decoded position"
    (Just (IfdDone (Just 1174675) "/nix/store/k8969n3iy9jyf2vjr2ylapb83bkrivwy-iserv-proxy-plan-to-nix-pkgs.drv" "built" 89477))
    (parseTrace "IFD #13: pid=1174675 /nix/store/k8969n3iy9jyf2vjr2ylapb83bkrivwy-iserv-proxy-plan-to-nix-pkgs.drv^out [built] 89477ms at \194\171unknown\194\187")
  check "IFD done, older format without pid, with a stack"
    (Just (IfdDone Nothing "/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv" "permanent-failure" 120))
    (parseTrace "IFD #2: /nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv^out [permanent-failure] 120ms at «unknown»\n  stack:\n    at foo.nix:1:1")
  check "IFD done, unknown status kept"
    (Just (IfdDone (Just 3) "/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv" "some-new-status" 5))
    (parseTrace "IFD #9: pid=3 /nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen-1793675984.drv^out [some-new-status] 5ms at «unknown»")
  check "IFD (cached) is not a trace" Nothing (parseTrace "IFD (cached): 1 derivation(s) skipped")
  check "worker-ready" (Just (WorkerReady 2145746 2656 54)) (parseTrace "nej-trace worker-ready pid=2145746 init_ms=2656 rss_mb=54")
  check "job"
    (Just (WorkerJob 2140038 3507 9 95 1921 "job" "x86_64-linux.unstable.ghc914-sh.aarch64-multiplatform.tests.cabal-22.shell"))
    (parseTrace "nej-trace job pid=2140038 ms=3507 ifds=9 ifd_ms=95 rss_mb=1921 kind=job attr=x86_64-linux.unstable.ghc914-sh.aarch64-multiplatform.tests.cabal-22.shell")
  check "job, error" (Just (WorkerJob 891229 62409 18 244 1824 "error" "x86_64-linux.unstable.ghc914-sh.wasi32.tests.with-packages.test-shell"))
    (parseTrace "nej-trace job pid=891229 ms=62409 ifds=18 ifd_ms=244 rss_mb=1824 kind=error attr=x86_64-linux.unstable.ghc914-sh.wasi32.tests.with-packages.test-shell")
  check "job, root attrs" (Just (WorkerJob 500423 0 0 0 14 "attrs" "")) (parseTrace "nej-trace job pid=500423 ms=0 ifds=0 ifd_ms=0 rss_mb=14 kind=attrs attr=")
  check "job, attr with a space" (Just (WorkerJob 1 2 0 0 3 "job" "a.\"b c\"")) (parseTrace "nej-trace job pid=1 ms=2 ifds=0 ifd_ms=0 rss_mb=3 kind=job attr=a.\"b c\"")
  check "worker-exit" (Just (WorkerExit 614258 "max-memory" 19017 2072))
    (parseTrace "nej-trace worker-exit pid=614258 reason=max-memory life_ms=19017 jobs=17 eval_ms=13586 ifd_ms=164 rss_mb=2072")
  check "building remote"
    (Just (Building "/nix/store/6wsbv91axs689l8an0cp09z4nhg5m66p-iserv-proxy-plan-to-nix-pkgs.drv" (Just "aarch64-darwin-6")))
    (parseTrace "building '/nix/store/6wsbv91axs689l8an0cp09z4nhg5m66p-iserv-proxy-plan-to-nix-pkgs.drv' on 'ssh://builder@aarch64-darwin-6.lan'...")
  check "building local"
    (Just (Building "/nix/store/3s3kivg8y5inn14jrpnzcvjh6cj1v6qm-x86_64-unknown-linux-musl-clang-wrapper-19.1.7.drv" Nothing))
    (parseTrace "building '/nix/store/3s3kivg8y5inn14jrpnzcvjh6cj1v6qm-x86_64-unknown-linux-musl-clang-wrapper-19.1.7.drv'...")
  check "a build log line is not a build" Nothing (parseTrace "building '/build/source/foo.c'...")
  check "build failed"
    (Just (BuildFailed "/nix/store/pq4s5c9c9lkx2q5mjkic5054capcii55-llvm-source-21.1.8-haskell.drv" (Just "aarch64-darwin-7")
      "Cannot build '/nix/store/pq4s5c9c9lkx2q5mjkic5054capcii55-llvm-source-21.1.8-haskell.drv'."))
    (parseTrace "error: build of '/nix/store/pq4s5c9c9lkx2q5mjkic5054capcii55-llvm-source-21.1.8-haskell.drv' on 'ssh://builder@aarch64-darwin-7.lan' failed: Cannot build '/nix/store/pq4s5c9c9lkx2q5mjkic5054capcii55-llvm-source-21.1.8-haskell.drv'.")
  check "build failed locally"
    (Just (BuildFailed "/nix/store/pq4s5c9c9lkx2q5mjkic5054capcii55-x.drv" Nothing "builder failed with exit code 1"))
    (parseTrace "error: build of '/nix/store/pq4s5c9c9lkx2q5mjkic5054capcii55-x.drv^out' failed: builder failed with exit code 1")

  check "evaluatorJobset" (Just ("ican-group-haskell-nix", "pullrequest-2618"))
    (evaluatorJobset "/nix/store/myzwgd3ylpl3d8zrl7kl3l97i7zl8mw6-perl-5.42.0/bin/perl /nix/store/5faqd5ckciw8md1h8bq7vms9n4lsmazw-hydra-0.1.20261102.327a257/bin/.hydra-eval-jobset-wrapped ican-group-haskell-nix pullrequest-2618")
  check "evaluatorJobset, not hydra-eval-jobset" Nothing (evaluatorJobset "/run/current-system/sw/bin/hydra-evaluator")
  check "builderName" ["aarch64-darwin-6", "x86_64-linux-0", "mac"]
    (map builderName ["ssh://builder@aarch64-darwin-6.lan", "ssh-ng://root@x86_64-linux-0.lan?compress=true", "mac"])

  -- The test evaluation: worker 1 builds the IFD while worker 2 waits on
  -- it, then reports it already valid after a longer wait.
  let at :: Double -> UTCTime
      at sec = addUTCTime (realToFrac sec) t0
      tr = parseAll at
        [ (0, "nej-trace worker-ready pid=1 init_ms=12 rss_mb=14")
        , (0, "nej-trace job pid=1 ms=0 ifds=0 ifd_ms=0 rss_mb=14 kind=attrs attr=")
        , (0, "nej-trace worker-ready pid=2 init_ms=11 rss_mb=14")
        , (1, "IFD start pid=2 n=1 drvs=/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv^out at «unknown»")
        , (2, "IFD start pid=1 n=1 drvs=/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv^out at «unknown»")
        , (2, "building '/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv'...")
        , (11, "IFD #1: pid=1 /nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv^out [built] 9409ms at «unknown»")
        , (11, "nej-trace job pid=1 ms=9413 ifds=1 ifd_ms=9409 rss_mb=19 kind=job attr=a")
        , (11, "nej-trace job pid=1 ms=2 ifds=0 ifd_ms=0 rss_mb=20 kind=job attr=plain")
        , (19, "IFD #1: pid=2 /nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv^out [already-valid] 17957ms at «unknown»")
        , (19, "nej-trace job pid=2 ms=17960 ifds=1 ifd_ms=17957 rss_mb=20 kind=job attr=b")
        , (20, "nej-trace worker-exit pid=1 reason=exit life_ms=20012 jobs=3 eval_ms=9415 ifd_ms=9409 rss_mb=20")
        ]
      drv = "/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv"
      (s, a) = applyTraces emptySnap tr
  -- Both workers waited from 2 s to 11 s.
  check "all waiting" 9000 (acAllWaitingMs a)
  check "live workers" [2] (M.keys (snWorkers s))
  check "nothing in flight" M.empty (snWaits s)
  check "ifds" (M.fromList [ ((drv, "built"), (1, [1], 9409)), ((drv, "already-valid"), (1, [2], 17957)) ])
    (M.map (\x -> (iaN x, S.toList (iaPids x), iaWaitMs x)) (acIfds a))
  check "worker 1" (Just (3, 9415, 1, 9409, Just "exit", Just 20012, Just 20, Just "plain"))
    ((\w -> (wdJobs w, wdEvalMs w, wdIfds w, wdIfdMs w, wdReason w, wdLifeMs w, wdMaxRss w, wdLastAttr w)) <$> M.lookup 1 (acWorkers a))
  check "worker 2 spawned" (Just (Just (addUTCTime (-0.011) t0))) (wdSpawned <$> M.lookup 2 (acWorkers a))
  check "builds" [(at 2, drv, Nothing, False, Nothing)] (acBuilds a)

  -- Split across two batches, a wait carries over in the snapshot, and the
  -- time with every worker blocked across the gap still counts.
  let (s1, a1) = applyTraces emptySnap (take 5 tr)
      (s2, a2) = applyTraces s1 (drop 5 tr)
  check "in flight after batch 1" [1, 2] (M.keys (snWaits s1))
  check "batches add up" (acAllWaitingMs a) (acAllWaitingMs a1 + acAllWaitingMs a2)
  check "batch 2 state" s s2

  -- A worker silent for over 10 minutes (crashed) does not keep the
  -- others from counting as all blocked.
  let quiet = parseAll at
        [ (0, "nej-trace worker-ready pid=1 init_ms=1 rss_mb=1")
        , (0, "nej-trace worker-ready pid=2 init_ms=1 rss_mb=1")
        , (700, "IFD start pid=2 n=1 drvs=/nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv^out at «unknown»")
        , (760, "IFD #1: pid=2 /nix/store/r65v6v8nc38fa6lpki080qzd9ppspmwb-ifd-gen.drv^out [built] 60000ms at «unknown»") ]
  check "a crashed worker is not counted" 60000 (acAllWaitingMs (snd (applyTraces emptySnap quiet)))

  n <- readIORef failures
  if n == 0 then putStrLn "all tests passed" else putStrLn (show n <> " failures") >> exitFailure

-- | Timed lines, all of which must parse.
parseAll :: (Double -> UTCTime) -> [(Double, Text)] -> [(UTCTime, Trace)]
parseAll at ls = [ (at sec, maybe (error ("unparsed: " <> show l)) id (parseTrace l)) | (sec, l) <- ls ]

t0 :: UTCTime
t0 = posixSecondsToUTCTime 1793675984
