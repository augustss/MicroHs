module ThrowMVar where
import Control.Concurrent
import System.Exit (ExitCode(..))

main :: IO ()
main = do
  mainTid <- myThreadId
  sig <- newEmptyMVar

  _ <- forkIO $ do
    putMVar sig ()              -- wake main: () parked in main's mt_mval
    putStrLn "throwing"
    throwTo mainTid ExitSuccess -- real exception parked in main's mt_exn

  () <- takeMVar sig

  putStrLn "should not print"
  return ()
