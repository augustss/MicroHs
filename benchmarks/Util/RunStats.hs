-- Parses the text a benchmark prints when run with "+RTS -v -RTS" into a
-- structured RunStats value. Deliberately hand-rolled (no Parsec): the
-- format is a fixed set of "<number> <label>" lines (see the PRINT calls
-- around src/runtime/eval.c:7240-7290), so a line-by-line classifier is
-- simpler than a general parser combinator would be here.
--
-- The stats block shares stdout with whatever the benchmark itself
-- printed, so parseRunStats scans every line of the whole capture and
-- simply ignores lines it doesn't recognize; that also means the parser
-- doesn't care where in the file the stats block starts, or in what
-- order its lines appear.
module Util.RunStats
  ( RunStats(..)
  , GCReductions(..)
  , SpecialReductions(..)
  , parseRunStats
  ) where
import Data.Char(isDigit, isSpace)
import Data.List(stripPrefix)
import Text.Read(readMaybe)

data RunStats = RunStats
  { rsCombFileSize            :: Integer
  , rsCellsAtStart            :: Integer
  , rsHeapCells               :: Integer
  , rsHeapBytes               :: Integer
  , rsCellsAllocated          :: Integer
  , rsAllocRateMBps           :: Maybe Double  -- Nothing if the run was too short to divide by (prints "inf")
  , rsGCs                     :: Integer
  , rsMaxCellsUsed            :: Integer
  , rsReductions              :: Integer
  , rsReductionRateMps        :: Maybe Double
  , rsYields                  :: Integer
  , rsResched                 :: Integer
  , rsArrayAlloc              :: Integer
  , rsArrayFree               :: Integer
  , rsForeignAlloc            :: Integer
  , rsForeignFree             :: Integer
  , rsBytestringAlloc         :: Integer
  , rsBytestringAllocMax      :: Integer
  , rsBytestringAllocBytes    :: Integer
  , rsBytestringAllocBytesMax :: Integer
  , rsBytestringFree          :: Integer
  , rsThreadCreate            :: Integer
  , rsThreadReap              :: Integer
  , rsStableptrAlloc          :: Integer
  , rsStableptrFree           :: Integer
  , rsWeakptrAlloc            :: Integer
  , rsWeakptrFree             :: Integer
  , rsTotalTimeSecs           :: Double
  , rsGcTimeSecs              :: Double
  , rsGcTimePercent           :: Maybe Double
  , rsGcMarkTimeSecs          :: Double
  , rsGcScanTimeSecs          :: Double
  , rsGCReductions            :: Maybe GCReductions       -- only present when the runtime is built with GCRED
  , rsSpecialReductions       :: Maybe SpecialReductions  -- only present when the runtime is built with GCRED
  }
  deriving (Show, Eq)

data GCReductions = GCReductions
  { gcrA, gcrK, gcrI, gcrInt, gcrFlip, gcrBI, gcrBxI, gcrCcBxI, gcrCC, gcrCcI, gcrCcBBCP :: Integer
  }
  deriving (Show, Eq)

data SpecialReductions = SpecialReductions
  { srBprime, srK4, srK3, srK2, srCcB, srZ, srR :: Integer
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------
-- Top level: scan every line, collect (key, value) contributions, then
-- assemble the record by looking each field up by name.

parseRunStats :: String -> Either String RunStats
parseRunStats text = do
  let contributions = map parseLine (lines text)
      ints    = concatMap fst contributions
      doubles = concatMap snd contributions
      needInt k = maybe (Left ("RunStats: missing field " ++ show k)) Right (lookup k ints)
      needDbl k = maybe (Left ("RunStats: missing field " ++ show k)) Right (lookup k doubles)
      optInt k = lookup k ints
      optDbl k = lookup k doubles
  combFileSize    <- needInt "comb_file_size"
  cellsAtStart    <- needInt "cells_at_start"
  heapCells       <- needInt "heap_cells"
  heapBytes       <- needInt "heap_bytes"
  cellsAllocated  <- needInt "cells_allocated"
  gcs             <- needInt "gcs"
  maxCellsUsed    <- needInt "max_cells_used"
  reductions      <- needInt "reductions"
  yields          <- needInt "yields"
  resched         <- needInt "resched"
  arrayAlloc      <- needInt "array_alloc"
  arrayFree       <- needInt "array_free"
  foreignAlloc    <- needInt "foreign_alloc"
  foreignFree     <- needInt "foreign_free"
  bsAlloc         <- needInt "bytestring_alloc"
  bsAllocMax      <- needInt "bytestring_alloc_max"
  bsAllocBytes    <- needInt "bytestring_alloc_bytes"
  bsAllocBytesMax <- needInt "bytestring_alloc_bytes_max"
  bsFree          <- needInt "bytestring_free"
  threadCreate    <- needInt "thread_create"
  threadReap      <- needInt "thread_reap"
  stableptrAlloc  <- needInt "stableptr_alloc"
  stableptrFree   <- needInt "stableptr_free"
  weakptrAlloc    <- needInt "weakptr_alloc"
  weakptrFree     <- needInt "weakptr_free"
  totalTime       <- needDbl "total_time_secs"
  gcTime          <- needDbl "gc_time_secs"
  gcMark          <- needDbl "gc_mark_secs"
  gcScan          <- needDbl "gc_scan_secs"
  let gcReds = do
        a     <- optInt "A"
        k     <- optInt "K"
        i     <- optInt "I"
        it    <- optInt "int"
        fl    <- optInt "flip"
        bi    <- optInt "BI"
        bxi   <- optInt "BxI"
        ccbxi <- optInt "C'BxI"
        cc    <- optInt "CC"
        cci   <- optInt "C'I"
        ccbc  <- optInt "C'BBCP"
        Just (GCReductions a k i it fl bi bxi ccbxi cc cci ccbc)
      specReds = do
        bp  <- optInt "B'"
        k4  <- optInt "K4"
        k3  <- optInt "K3"
        k2  <- optInt "K2"
        ccb <- optInt "C'B"
        z   <- optInt "Z"
        r   <- optInt "R"
        Just (SpecialReductions bp k4 k3 k2 ccb z r)
  return RunStats
    { rsCombFileSize            = combFileSize
    , rsCellsAtStart            = cellsAtStart
    , rsHeapCells               = heapCells
    , rsHeapBytes               = heapBytes
    , rsCellsAllocated          = cellsAllocated
    , rsAllocRateMBps           = optDbl "alloc_rate_mbps"
    , rsGCs                     = gcs
    , rsMaxCellsUsed            = maxCellsUsed
    , rsReductions              = reductions
    , rsReductionRateMps        = optDbl "reduction_rate_mps"
    , rsYields                  = yields
    , rsResched                 = resched
    , rsArrayAlloc              = arrayAlloc
    , rsArrayFree               = arrayFree
    , rsForeignAlloc            = foreignAlloc
    , rsForeignFree             = foreignFree
    , rsBytestringAlloc         = bsAlloc
    , rsBytestringAllocMax      = bsAllocMax
    , rsBytestringAllocBytes    = bsAllocBytes
    , rsBytestringAllocBytesMax = bsAllocBytesMax
    , rsBytestringFree          = bsFree
    , rsThreadCreate            = threadCreate
    , rsThreadReap              = threadReap
    , rsStableptrAlloc          = stableptrAlloc
    , rsStableptrFree           = stableptrFree
    , rsWeakptrAlloc            = weakptrAlloc
    , rsWeakptrFree             = weakptrFree
    , rsTotalTimeSecs           = totalTime
    , rsGcTimeSecs              = gcTime
    , rsGcTimePercent           = optDbl "gc_time_percent"
    , rsGcMarkTimeSecs          = gcMark
    , rsGcScanTimeSecs          = gcScan
    , rsGCReductions            = gcReds
    , rsSpecialReductions       = specReds
    }

--------------------------------------------------------------------------
-- Per-line classification. Every recognized line contributes zero or more
-- (key, value) pairs; unrecognized lines (the benchmark's own output)
-- contribute nothing.

parseLine :: String -> ([(String, Integer)], [(String, Double)])
parseLine raw =
  let line = dropWhile isSpace raw
  in case tryTotalTime line of
       Just t -> ([], [("total_time_secs", t)])
       Nothing -> case tryGcTime line of
         Just (gcSecs, pct, markSecs, scanSecs) ->
           ( []
           , [("gc_time_secs", gcSecs), ("gc_mark_secs", markSecs), ("gc_scan_secs", scanSecs)]
             ++ maybe [] (\p -> [("gc_time_percent", p)]) pct
           )
         Nothing -> case tryKVLine "GC reductions " line of
           Just kvs -> (kvs, [])
           Nothing -> case tryKVLine "special reductions " line of
             Just kvs -> (kvs, [])
             Nothing -> case numPrefix line of
               Just (n, rest) -> classifyNumberedLine n rest
               Nothing        -> ([], [])

-- "<int> <label...>" lines, e.g. "         31108 combinator file size" or
-- "       50000000 cells heap size (800000000 bytes)".
classifyNumberedLine :: Integer -> String -> ([(String, Integer)], [(String, Double)])
classifyNumberedLine n rest
  | rest == "combinator file size" = ([("comb_file_size", n)], [])
  | rest == "cells at start"       = ([("cells_at_start", n)], [])
  | Just tl <- stripPrefix "cells heap size (" rest
      = ([("heap_cells", n)] ++ intField "heap_bytes" tl, [])
  | Just tl <- stripPrefix "cells allocated (" rest
      = ([("cells_allocated", n)], rateField "alloc_rate_mbps" tl)
  | rest == "GCs"                  = ([("gcs", n)], [])
  | rest == "max cells used"       = ([("max_cells_used", n)], [])
  | Just tl <- stripPrefix "reductions (" rest
      = ([("reductions", n)], rateField "reduction_rate_mps" tl)
  | Just tl <- stripPrefix "yields (" rest
      = ([("yields", n)] ++ intField "resched" tl, [])
  | rest == "array alloc"          = ([("array_alloc", n)], [])
  | rest == "array free"           = ([("array_free", n)], [])
  | rest == "foreign alloc"        = ([("foreign_alloc", n)], [])
  | rest == "foreign free"         = ([("foreign_free", n)], [])
  | Just tl <- stripPrefix "bytestring alloc bytes (max " rest
      = ([("bytestring_alloc_bytes", n)] ++ intField "bytestring_alloc_bytes_max" tl, [])
  | Just tl <- stripPrefix "bytestring alloc (max " rest
      = ([("bytestring_alloc", n)] ++ intField "bytestring_alloc_max" tl, [])
  | rest == "bytestring free"      = ([("bytestring_free", n)], [])
  | rest == "thread create"        = ([("thread_create", n)], [])
  | rest == "thread reap"          = ([("thread_reap", n)], [])
  | rest == "stableptr alloc"      = ([("stableptr_alloc", n)], [])
  | rest == "stableptr free"       = ([("stableptr_free", n)], [])
  | rest == "weakptr alloc"        = ([("weakptr_alloc", n)], [])
  | rest == "weakptr free"         = ([("weakptr_free", n)], [])
  | otherwise                      = ([], [])
  where
    intField key s = maybe [] (\v -> [(key, v)]) (readMaybe (takeWhile isDigit s))
    rateField key s = maybe [] (\v -> [(key, v)]) (parseRate (takeWhile (/= ' ') s))

-- The number (with optional ',' thousands separators) at the start of a
-- line, and everything after it with leading spaces dropped.
numPrefix :: String -> Maybe (Integer, String)
numPrefix s =
  let (digitsPart, rest) = span (\c -> isDigit c || c == ',') s
      cleaned = filter (/= ',') digitsPart
  in if null cleaned then Nothing else fmap (\v -> (v, dropWhile isSpace rest)) (readMaybe cleaned)

-- A leading floating-point literal (mhs prints these with a plain '.',
-- never scientific notation) and the rest of the string.
takeFloat :: String -> Maybe (Double, String)
takeFloat s =
  let (numPart, rest) = span (\c -> isDigit c || c == '.') s
  in if null numPart then Nothing else fmap (\v -> (v, rest)) (readMaybe numPart)

-- Rates can print as "inf" when the run is too short to divide by (see
-- num_alloc * NODE_SIZE / (run_time / 1000) at src/runtime/eval.c:7248).
parseRate :: String -> Maybe Double
parseRate "inf" = Just (1 / 0)
parseRate s     = readMaybe s

-- "2.03s total expired time"
tryTotalTime :: String -> Maybe Double
tryTotalTime line = do
  (secs, r1) <- takeFloat line
  r2 <- stripPrefix "s" r1
  if dropWhile isSpace r2 == "total expired time" then Just secs else Nothing

-- "0.00s gc expired time = 0.1% (0.00s mark + 0.00s scan)"
tryGcTime :: String -> Maybe (Double, Maybe Double, Double, Double)
tryGcTime line = do
  (gcSecs, r1) <- takeFloat line
  r2 <- stripPrefix "s" r1
  r3 <- stripPrefix "gc expired time = " (dropWhile isSpace r2)
  let (pctTok, r4) = span (/= '%') r3
  r5 <- stripPrefix "%" r4
  r6 <- stripPrefix "(" (dropWhile isSpace r5)
  (markSecs, r7) <- takeFloat r6
  r8 <- stripPrefix "s mark + " r7
  (scanSecs, r9) <- takeFloat r8
  _ <- stripPrefix "s scan)" r9
  return (gcSecs, parseRate pctTok, markSecs, scanSecs)

-- " GC reductions A=405, K=13, ..." / " special reductions B'=0 K4=0 ..."
-- Separators are an inconsistent mix of ", " and " " in the C format
-- string, so commas are normalized to spaces before splitting on
-- whitespace, then each "key=value" token is split on '='.
tryKVLine :: String -> String -> Maybe [(String, Integer)]
tryKVLine label line = do
  rest <- stripPrefix label line
  let cleaned = map (\c -> if c == ',' then ' ' else c) rest
      toks    = words cleaned
  Just [ (k, v) | tok <- toks
                , let (k, eqv) = break (== '=') tok
                , not (null eqv)
                , Just v <- [readMaybe (drop 1 eqv)] ]
