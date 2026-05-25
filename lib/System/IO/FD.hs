module System.IO.FD (waitForReadFD, waitForWriteFD) where
import Primitives
import Foreign.C.Error

waitForReadFD :: Int -> IO ()
waitForReadFD = throwErrnoIfMinus1_ "waitForReadFD" . primWaitReadFD

waitForWriteFD :: Int -> IO ()
waitForWriteFD = throwErrnoIfMinus1_ "waitForWriteFD" . primWaitWriteFD
