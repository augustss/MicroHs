-- Runs every benchmark's compiled executable multiple times, parses
-- the stats with RunStats, and writes one JSON file per invocation
-- to benchmarks/results/. Running it again writes a new, separate file
-- (named by the run's timestamp).
--
-- Expects the benchmarks to already be built, and expects to be run from
-- the repo root
module Util.BenchHarness(main) where
import Control.Exception(SomeException, catch)
import Data.List(dropWhileEnd, minimumBy, sort)
import Data.Char(isSpace)
import Data.Ord(comparing)
import Numeric(showFFloat)
import System.Directory(createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO.TimeMilli(getTimeMilli)
import System.Process(readProcess)
import Text.Read(readMaybe)

import Util.Json
import Util.RunStats

-- | The directory in which we place singular .hs-files, which
-- each encode a benchmark
benchmarkDirectory :: String
benchmarkDirectory = "benchmarks/Loads/"

-- | How many times to run each benchmark's executable
defaultRuns :: Int
defaultRuns = 5

-- | Read the names of all benchmark programs in benchmarkDirectory
readBenchmarkNames :: IO [String]
readBenchmarkNames = do
  files <- listDirectory benchmarkDirectory
  return $ map (takeWhile ((/=) '.')) files

main :: IO ()
main = do
  args <- getArgs
  runs <- case args of
    [s] | Just n <- readMaybe s, n > 0 -> return n
    []                                 -> return defaultRuns
    _                                  -> do
      putStrLn "usage: BenchHarness.exe [runs]"
      exitFailure

  -- the time at which the benchmark runner was invoked, for
  -- tagging the report
  ts <- getTimeMilli

  names <- readBenchmarkNames

  -- meta-information for the report
  commit <- getGitCommit
  dirty  <- getGitDirty

  -- run the benchmarks
  entries <- mapM (runOne runs) names

  let anyFail = any entryFailed entries
      doc = JObject
        [ ("timestamp_ms", jInt ts)
        , ("git_commit", jMaybe jString commit)
        , ("git_dirty", jMaybe JBool dirty)
        , ("benchmarks", JArray (map entryJSON entries))
        ]
      path = "benchmarks/results/run-" ++ show ts ++ ".json"

  -- write the report
  createDirectoryIfMissing True "benchmarks/results"
  writeFile path (renderJSON doc ++ "\n")

  -- briefly output
  mapM_ (putStrLn . describeEntry) entries
  putStrLn ("wrote " ++ path)

  if anyFail then exitFailure else return ()

--------------------------------------------------------------------------

-- | An entry for the benchmark report
data Entry
    -- ^ All went well; one RunStats per repeat (in run order), always
    -- non-empty
  = EntryOk String [RunStats]
    -- ^ The entry was not built, for some reason
  | EntryMissing String
    -- ^ Entry existed and ran, but its output could not be parsed
    -- I am not sure when this can happen, but there's a case for it none the less.
  | EntryParseError String String

-- | Does an entry represent a benchmark that could not be run?
entryFailed :: Entry -> Bool
entryFailed (EntryOk _ _) = False
entryFailed _             = True

-- | Turn an entry into a JSON object, for the report
entryJSON :: Entry -> JSON
entryJSON (EntryOk name stats)       = JObject [("name", jString name), ("ok", JBool True),  ("stats", statsJSON stats)]
entryJSON (EntryMissing name)        = JObject [("name", jString name), ("ok", JBool False), ("error", jString "executable not built")]
entryJSON (EntryParseError name err) = JObject [("name", jString name), ("ok", JBool False), ("error", jString err)]

-- | Turn an entry into a brief one-line description
describeEntry :: Entry -> String
describeEntry (EntryOk name stats) =
  let times = map rsTotalTimeSecs stats
      rs    = fastest stats
  in name ++ ": " ++ show (rsReductions rs) ++ " reductions, min " ++
     showSecs (minimum times) ++ "s (median " ++ showSecs (median times) ++ "s, n=" ++ show (length stats) ++ "), " ++
     show (rsGCs rs) ++ " GCs"
describeEntry (EntryMissing name)       = name ++ ": FAIL (not built -- run 'make build' first)"
describeEntry (EntryParseError name e)  = name ++ ": FAIL (" ++ e ++ ")"

-- | Run a benchmark N times and collect its results. Stops at the first
-- repeat whose output fails to parse.
runOne :: Int -> String -> IO Entry
runOne runs name = do
  built <- doesFileExist exe
  if not built then return (EntryMissing name) else go []
  where
    exe = "benchmarks/" ++ name ++ ".exe"
    go acc
      | length acc == runs = return (EntryOk name (reverse acc))
      | otherwise = do
          out <- readProcess exe ["+RTS", "-v", "-RTS"] ""
          case parseRunStats out of
            Left err -> return (EntryParseError name err)
            Right rs -> go (rs : acc)

-- | The repeat with the lowest total time
fastest :: [RunStats] -> RunStats
fastest = minimumBy (comparing rsTotalTimeSecs)

showSecs :: Double -> String
showSecs d = showFFloat (Just 3) d ""

median :: [Double] -> Double
median xs =
  let sorted = sort xs
      n      = length sorted
  in if odd n
     then sorted !! (n `div` 2)
     else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2

--------------------------------------------------------------------------
-- [RunStats] -> JSON. Deterministic fields come from the fastest repeat;
-- total_time_secs/gc_time_secs become {min, median, samples} instead of a
-- single number.

statsJSON :: [RunStats] -> JSON
statsJSON stats =
  let rs      = fastest stats
      times   = map rsTotalTimeSecs stats
      gcTimes = map rsGcTimeSecs stats
  in JObject
  [ ("runs", jInt (length stats))
  , ("comb_file_size", jInt (rsCombFileSize rs))
  , ("cells_at_start", jInt (rsCellsAtStart rs))
  , ("heap_cells", jInt (rsHeapCells rs))
  , ("heap_bytes", jInt (rsHeapBytes rs))
  , ("cells_allocated", jInt (rsCellsAllocated rs))
  , ("alloc_rate_mbps", jMaybe jDouble (rsAllocRateMBps rs))
  , ("gcs", jInt (rsGCs rs))
  , ("max_cells_used", jInt (rsMaxCellsUsed rs))
  , ("reductions", jInt (rsReductions rs))
  , ("reduction_rate_mps", jMaybe jDouble (rsReductionRateMps rs))
  , ("yields", jInt (rsYields rs))
  , ("resched", jInt (rsResched rs))
  , ("array_alloc", jInt (rsArrayAlloc rs))
  , ("array_free", jInt (rsArrayFree rs))
  , ("foreign_alloc", jInt (rsForeignAlloc rs))
  , ("foreign_free", jInt (rsForeignFree rs))
  , ("bytestring_alloc", jInt (rsBytestringAlloc rs))
  , ("bytestring_alloc_max", jInt (rsBytestringAllocMax rs))
  , ("bytestring_alloc_bytes", jInt (rsBytestringAllocBytes rs))
  , ("bytestring_alloc_bytes_max", jInt (rsBytestringAllocBytesMax rs))
  , ("bytestring_free", jInt (rsBytestringFree rs))
  , ("thread_create", jInt (rsThreadCreate rs))
  , ("thread_reap", jInt (rsThreadReap rs))
  , ("stableptr_alloc", jInt (rsStableptrAlloc rs))
  , ("stableptr_free", jInt (rsStableptrFree rs))
  , ("weakptr_alloc", jInt (rsWeakptrAlloc rs))
  , ("weakptr_free", jInt (rsWeakptrFree rs))
  , ("total_time_secs", timingJSON times)
  , ("gc_time_secs", timingJSON gcTimes)
  , ("gc_time_percent", jMaybe jDouble (rsGcTimePercent rs))
  , ("gc_mark_time_secs", jDouble (rsGcMarkTimeSecs rs))
  , ("gc_scan_time_secs", jDouble (rsGcScanTimeSecs rs))
  , ("gc_reductions", jMaybe gcReductionsJSON (rsGCReductions rs))
  , ("special_reductions", jMaybe specialReductionsJSON (rsSpecialReductions rs))
  ]

timingJSON :: [Double] -> JSON
timingJSON samples = JObject
  [ ("min", jDouble (minimum samples))
  , ("median", jDouble (median samples))
  , ("samples", JArray (map jDouble samples))
  ]

gcReductionsJSON :: GCReductions -> JSON
gcReductionsJSON r = JObject
  [ ("A", jInt (gcrA r)), ("K", jInt (gcrK r)), ("I", jInt (gcrI r))
  , ("int", jInt (gcrInt r)), ("flip", jInt (gcrFlip r)), ("BI", jInt (gcrBI r))
  , ("BxI", jInt (gcrBxI r)), ("C'BxI", jInt (gcrCcBxI r)), ("CC", jInt (gcrCC r))
  , ("C'I", jInt (gcrCcI r)), ("C'BBCP", jInt (gcrCcBBCP r))
  ]

specialReductionsJSON :: SpecialReductions -> JSON
specialReductionsJSON r = JObject
  [ ("B'", jInt (srBprime r)), ("K4", jInt (srK4 r)), ("K3", jInt (srK3 r))
  , ("K2", jInt (srK2 r)), ("C'B", jInt (srCcB r)), ("Z", jInt (srZ r)), ("R", jInt (srR r))
  ]

--------------------------------------------------------------------------
-- Best-effort git metadata; harness still runs (with null commit/dirty
-- fields) if git isn't available or this isn't a git checkout.

getGitCommit :: IO (Maybe String)
getGitCommit =
  (Just . trim <$> readProcess "git" ["rev-parse", "HEAD"] "")
  `catch` \(_ :: SomeException) -> return Nothing

getGitDirty :: IO (Maybe Bool)
getGitDirty =
  (Just . not . null . trim <$> readProcess "git" ["status", "--porcelain"] "")
  `catch` \(_ :: SomeException) -> return Nothing

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
