module System.Mem(performGC, performGCWithReduction) where
import qualified Prelude()
import MiniPrelude
import Primitives

performGC :: IO ()
performGC = primGC 0

performGCWithReduction :: IO ()
performGCWithReduction = primGC 1
