module Loads.ForkJoin(main) where
import Control.Concurrent

-- Fork many green threads that each do a chunk of CPU work, then join on
-- all of them via MVar. Exercises thread creation/scheduling/reap and MVar
-- synchronization.
work :: Int -> Int
work = go 0
  where
    go !acc 0 = acc
    go !acc k = go (acc + k) (k - 1)

worker :: MVar Int -> Int -> IO ()
worker result n = putMVar result (work n)

main :: IO ()
main = do
  let numWorkers    = 50
      workPerThread = 50000
  mvars <- mapM (const newEmptyMVar) [1 .. numWorkers]
  mapM_ (\m -> forkIO (worker m workPerThread)) mvars
  results <- mapM takeMVar mvars
  print (sum results)
