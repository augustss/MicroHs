module ThrowMVar where
import Control.Concurrent
import System.Exit (ExitCode(..))

main :: IO ()
main = do
  mainTid <- myThreadId
  sig <- newEmptyMVar

  _ <- forkIO $ do
    putMVar sig ()              -- wake main: () parked in main's mt_mval
    throwTo mainTid ExitSuccess -- real exception parked in main's mt_exn

  () <- takeMVar sig

  -- This is not printed, since the exception has been delivered
  putStrLn "BUG NOT TRIGGERED"