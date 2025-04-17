module Con(main) where
import Prelude
import Control.Concurrent
import System.IO

delay :: Int -> IO ()
delay 0 = return ()
delay n = delay (n - 1 :: Int)

run :: Int -> String -> IO ()
run 0 _ = return ()
run i s = do
  delay 1000
  putStr s
  hFlush stdout
  run (i-1) s

main :: IO ()
main = do
  putStrLn "start"
  forkIO $ run 1000 "b"
  forkIO $ run 1000 "c"
  run 1000 "a"
  delay 3000
  putStrLn "\ndone"
