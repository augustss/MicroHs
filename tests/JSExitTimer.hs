-- The program ends when main does, even though another thread is still in
-- threadDelay: the timer the runtime armed for that thread must not fire into
-- the stopped runtime after exit() (an uncaught error in the JavaScript event
-- loop, exit status 1).  Expected: the two lines below, exit status 0, and no
-- wait for the delay (emscripten target).
module JSExitTimer(main) where
import Control.Concurrent
import GHC.Wasm.Prim

foreign import javascript "setTimeout(() => $1(), $2)" later :: JSVal -> Int -> IO ()

main :: IO ()
main = do
  mv <- newEmptyMVar
  _ <- forkIO $ threadDelay 2000000 >> putStrLn "the sleeping thread ran"
  cb <- syncCallback $ putMVar mv ()
  later cb 10
  putStrLn "main: waiting"      -- every thread blocks: a 2s timer is armed, and the event loop runs the callback
  takeMVar mv
  freeJSVal cb                  -- so no callback keeps the program alive after main
  putStrLn "main: done"
