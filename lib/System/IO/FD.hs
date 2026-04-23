module System.IO.FD (waitForReadFD, waitForWriteFD) where

import Primitives

waitForReadFD :: Int -> IO ()
waitForReadFD = primWaitReadFD

waitForWriteFD :: Int -> IO ()
waitForWriteFD = primWaitWriteFD