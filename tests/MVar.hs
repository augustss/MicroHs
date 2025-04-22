module MVar where
import Control.Concurrent
import Control.Concurrent.MVar

delay :: Int -> IO ()
delay i = loop i `seq` return ()

loop :: Int -> Int
loop i = if i == 0 then 0 else loop (i-1)

fun :: MVar Int -> IO ()
fun mv = do
  putStrLn "fun A"
  i <- takeMVar mv
  print i
  putStrLn "fun B"

main :: IO ()
main = do
  putStrLn "main A"
  mvar <- newEmptyMVar
  forkIO (fun mvar)
  putStrLn "main B"
  delay 100000
  putStrLn "main C"
  putMVar mvar 999
  putStrLn "main D"
  putMVar mvar 888
  putStrLn "main E"
  forkIO (delay 100000 >> fun mvar)
  putMVar mvar 777
  putStrLn "main F"
  delay 100000
  fun mvar
  delay 100000
  putStrLn "main G"

