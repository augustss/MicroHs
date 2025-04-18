module Con(main) where
import Prelude
import Control.Concurrent
import System.IO

delay :: Int -> IO ()
delay i = loop i `seq` return ()
{-
delay 0 = return ()
delay n = do
  return ()
  delay (n - 1 :: Int)
-}

loop :: Int -> Int
loop i = if i == 0 then 0 else loop (i-1)

run :: Int -> String -> IO ()
run 0 _ = return ()
run i s = do
  delay 10000
  putStr s
  hFlush stdout
  run (i-1) s

xrun :: String -> IO ()
xrun s = do
--  delay 1000
  i <- myThreadId
  putStrLn $ "thread " ++ show i ++ ": " ++ show s
  run 1000 s

main :: IO ()
main = do
  putStrLn "start"
  forkIO $ xrun "b"
  forkIO $ xrun "c"
  xrun "a"
  delay 3000
  putStrLn "\ndone"
