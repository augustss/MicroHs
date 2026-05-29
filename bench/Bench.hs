module Bench(main) where
import System.IO.TimeMilli(getTimeMilli)

-- recursion + int arithmetic (dispatch + alloc bound)
nfib :: Int -> Int
nfib n = if n < 2 then 1 else nfib (n-1) + nfib (n-2) + 1

-- Takeuchi: deep recursion, lots of comparisons/subtraction
tak :: Int -> Int -> Int -> Int
tak x y z = if y < x then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
            else z

-- list allocation + strict fold (GC pressure)
sumTo :: Int -> Int
sumTo n = go 0 (enumFromTo 1 n)
  where go acc [] = acc
        go acc (x:xs) = let a = acc+x in a `seq` go a xs

-- build + reverse + sum repeatedly (allocation churn + GC pressure)
churn :: Int -> Int -> Int
churn 0 acc = acc
churn k acc =
  let s = mysum (myreverse (enumFromTo k (k+1000)))
      acc' = (acc + s) `rem` 2000000000
  in acc' `seq` churn (k-1) acc'

mysum :: [Int] -> Int
mysum = go 0 where go a [] = a; go a (x:xs) = let b = a+x in b `seq` go b xs

myreverse :: [Int] -> [Int]
myreverse = go [] where go a [] = a; go a (x:xs) = go (x:a) xs

timeIt :: String -> Int -> IO ()
timeIt name v = do
  t1 <- getTimeMilli
  r <- v `seq` return v
  t2 <- getTimeMilli
  putStrLn (name ++ ": " ++ show r ++ "  (" ++ show (t2-t1) ++ " ms)")

main :: IO ()
main = do
  timeIt "nfib32" (nfib 32)
  timeIt "tak"    (tak 24 16 8)
  timeIt "sumTo"  (sumTo 5000000)
  timeIt "churn"  (churn 4000 0)
