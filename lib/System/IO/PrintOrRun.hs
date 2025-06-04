-- Copyright 2023-2025 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.PrintOrRun(PrintOrRun(..), _withArgs) where
import Primitives(primNumAlloc)
import System.Environment
import System.IO.TimeMilli

-- Helper for interactive system
class PrintOrRun a where
  _printOrRun :: a -> IO ()
  _printOrRunStats :: a -> IO ()

instance PrintOrRun (IO ()) where
  _printOrRun a = a
  _printOrRunStats a = do
    t1 <- getTimeMilli
    a1 <- primNumAlloc
    r <- a
    a2 <- primNumAlloc
    t2 <- getTimeMilli
    putStrLn $ "(" ++ show (t2 - t1) ++ "ms, " ++ show (a2 - a1) ++ " cells)"
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
