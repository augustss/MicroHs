-- Copyright 2023-2026 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.PrintOrRun(PrintOrRun(..), _withArgs) where
import System.CPUTime
import System.Environment
import System.IO.TimeMilli
import System.RTS

-- Helper for interactive system
class PrintOrRun a where
  _printOrRun :: a -> IO ()
  _printOrRunStats :: a -> IO ()

instance PrintOrRun (IO ()) where
  _printOrRun a = a
  _printOrRunStats a = do
    t1 <- getTimeMilli
    s1 <- getStats
    c1 <- getCPUTime
    r <- a
    c2 <- getCPUTime
    s2 <- getStats
    t2 <- getTimeMilli
    putStrLn $ "(" ++ show (t2 - t1) ++ "ms (elapsed), " ++
                show ((c2 - c1) `div` 1000_000_000) ++ "ms (cpu), " ++
                show (reductions s2 - reductions s1) ++ " reds, " ++
                show (cellsAllocated s2 - cellsAllocated s1) ++ " cells)"
    return r;

{-  Resolution of overlapping instances is not good enough for this.  Yet.
instance Show a => PrintOrRun (IO a) where
  printOrRun a = a `primBind` print
-}

instance Show a => PrintOrRun a where
  _printOrRun = print
  _printOrRunStats a = _printOrRunStats (print a)

_withArgs :: [String] -> IO () -> IO ()
_withArgs = withArgs
