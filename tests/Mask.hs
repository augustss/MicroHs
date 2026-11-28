module Mask where
import Control.Concurrent
import Control.Exception

printMaskingState :: String -> IO ()
printMaskingState s = putStr s >> getMaskingState >>= print

main :: IO ()
main = do
  printMaskingState "start "
  catch (do
            printMaskingState "catch start "
            uninterruptibleMask_ $ do
              printMaskingState "unintr start "
              error "a"
        )
        (\ (_ :: SomeException) -> do
            printMaskingState "handler "
        )
  printMaskingState "end "
  -- The thread is rescheduled inside the mask, and then the handler
  -- must still restore the masking state from when catch was called.
  catch (mask_ $ do
            threadDelay 1000
            printMaskingState "rescheduled "
            error "b"
        )
        (\ (_ :: SomeException) -> do
            printMaskingState "handler "
        )
  printMaskingState "end "
