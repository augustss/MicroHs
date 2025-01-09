module Control.Concurrent(
  threadDelay
  ) where

foreign import ccall "usleep" c_delay :: Int -> IO ()

-- | Suspends the current thread for a given number of microseconds.
threadDelay :: Int -> IO ()
threadDelay = c_delay
