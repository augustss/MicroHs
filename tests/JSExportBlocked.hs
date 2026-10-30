-- A foreign export called from JavaScript while main is blocked runs as a thread of
-- its own: main continues, and the program ends when main does (emscripten target).
module JSExportBlocked(main) where
import Control.Concurrent
import System.IO.Unsafe

foreign export javascript ping :: Int -> IO Int
foreign import javascript "setTimeout(() => console.log('js: ping -> ' + Module._ping(41)), 10)" schedule :: IO ()

box :: MVar Int
box = unsafePerformIO newEmptyMVar
{-# NOINLINE box #-}

ping :: Int -> IO Int
ping x = do
  putStrLn ("hs: ping " ++ show x)
  putMVar box x
  pure (x + 1)

main :: IO ()
main = do
  schedule
  let loop :: Int -> IO ()
      loop n = do
        r <- tryTakeMVar box
        case r of
          Just v -> putStrLn ("main: got " ++ show v)
          Nothing | n == 0 -> putStrLn "main: timeout"
                  | otherwise -> threadDelay 5000 >> loop (n - 1)
  loop 100
  threadDelay 20000
  putStrLn "main: done"
