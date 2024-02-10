module System.IO.Unsafe(unsafePerformIO) where
import Prelude()
import Primitives

unsafePerformIO :: IO a -> a
unsafePerformIO = primPerformIO
