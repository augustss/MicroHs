module Con(main) where
import Prelude
import Control.Concurrent
import System.IO

delay :: Int -> IO ()
delay i = loop i `seq` return ()

loop :: Int -> Int
loop i = if i == 0 then 0 else loop (i-1)

run :: Int -> String -> IO ()
run 0 _ = return ()
run i s = do
  delay 10000
  putStr s
  hFlush stdout
  run (i-1) s

xrun :: Int -> String -> IO ()
xrun i s = do
  delay i
  i <- myThreadId
  putStrLn $ "thread " ++ show i ++ ": " ++ show s
  delay 2000
  run 1000 s

main :: IO ()
main = do
  putStrLn "start"
  forkIO $ xrun 1000 "b"
  forkIO $ xrun 2000 "c"
  xrun 0 "a"
  delay 3000
  putStrLn "\ndone"
