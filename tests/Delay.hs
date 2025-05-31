module Delay where
import Control.Concurrent
import System.IO.TimeMilli

timedDelay :: String -> Int -> IO ()
timedDelay msg usecs = do
  t1 <- getTimeMilli
  threadDelay usecs
  t2 <- getTimeMilli
  let dt = t2 - t1
      rdt = (dt `div` 20) * 20 -- round down ms
  putStrLn $ msg ++ " " ++ show usecs ++ " " ++ show rdt ++ "ms"

thr1 :: IO ()
thr1 = do
  timedDelay "thr1" (100000::Int)

thr2 :: IO ()
thr2 = do
  timedDelay "thr2" (300000::Int)

main :: IO ()
main = do
  forkIO thr1
  forkIO thr2
  timedDelay "main" (200000::Int)
  timedDelay "end" (500000::Int)
