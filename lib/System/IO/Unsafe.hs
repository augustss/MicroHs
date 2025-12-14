module System.IO.Unsafe(unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO) where
import qualified Prelude()
import Primitives

unsafePerformIO :: IO a -> a
unsafePerformIO = primPerformIO

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = primPerformIO

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO ioa = primReturn (primPerformIO ioa)
