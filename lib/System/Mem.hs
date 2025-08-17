module System.Mem(performGC) where
import qualified Prelude()
import MiniPrelude
import Primitives

performGC :: IO ()
performGC = primGC
