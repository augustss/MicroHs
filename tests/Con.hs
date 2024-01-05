module Con(main) where
import Prelude
import Control.Concurrent
import System.IO

run :: String -> IO ()
run s = do
  let loop 0 = return ()
      loop n = loop (n - 1 :: Int)
  loop 1000
  putStr s
  hFlush stdout
  run s

main :: IO ()
main = do
  forkIO $ run "b"
  forkIO $ run "c"
  run "a"
