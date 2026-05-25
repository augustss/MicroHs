module System.IO.FD (waitForReadFD, waitForWriteFD) where

import Primitives

waitForReadFD :: Int -> IO Int
waitForReadFD = primWaitReadFD

waitForWriteFD :: Int -> IO Int
waitForWriteFD = primWaitWriteFD
