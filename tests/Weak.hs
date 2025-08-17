module Weak where
import System.Mem.Weak
import System.Mem
import Data.IORef
import Control.Concurrent

main :: IO ()
main = do
  let key = "key" :: String
  let val = "val" :: String
  let final = putStrLn "run final"
  putStrLn $ "before first gc " ++ show (key, val)
  --putStr $ "test finalizer: "; final
  wk <- mkWeak key val (Just final)
  putStrLn "mkWeak done"
  performGC  -- key is alive below, so the wek pointer should survive
  mval <- deRefWeak wk
  putStrLn $ "after first gc " ++ show (key, mval)
  performGC  -- the key is not alive, so the weak pointer should be die
  mval' <- deRefWeak wk
  putStrLn $ "after second gc " ++ show mval'
  yield     -- give finalizer a chance to run
  putStrLn "after yield"

  let key2 = "key2" :: String
  let final2 = putStrLn "run final2"
  wk2 <- mkWeak key2 val (Just final2)
  putStrLn "about to finalize"
  finalize wk2
  finalize wk2
  performGC
