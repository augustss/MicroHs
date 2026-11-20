-- A freed callback does not keep the program alive: a deadlock is still reported (emscripten target).
module JSDeadlock(main) where
import Control.Concurrent.MVar
import GHC.Wasm.Prim

main :: IO ()
main = do
  cb <- syncCallback (pure ())
  freeJSVal cb
  putStrLn "before the deadlock"
  m <- newEmptyMVar :: IO (MVar ())
  takeMVar m
