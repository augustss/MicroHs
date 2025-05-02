-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.PrintOrRun(PrintOrRun(..), _withArgs) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Char(String)
import System.Environment
import System.IO
import Text.Show

-- Helper for interactive system
class PrintOrRun a where
  printOrRun :: a -> IO ()

instance PrintOrRun (IO ()) where
  printOrRun a = a

instance forall a . Show a => PrintOrRun a where
  printOrRun = print

_withArgs :: [String] -> IO () -> IO ()
_withArgs = withArgs
