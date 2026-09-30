module Loads.MergeSort(main) where
import Data.List(foldl')

-- Clanker-proposed deterministic PRNG so the benchmark needs no external
-- dependency and is reproducible across runs/machines.
lcg :: Int -> Int
lcg x = (1103515245 * x + 12345) `mod` 2147483648

genList :: Int -> Int -> [Int]
genList _    0 = []
genList seed n = seed : genList (lcg seed) (n - 1)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge xs@(x : xs') ys@(y : ys')
  | x <= y    = x : merge xs' ys
  | otherwise = y : merge xs ys'

msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort as) (msort bs)
  where (as, bs) = splitAt (length xs `quot` 2) xs

main :: IO ()
main = do
  let n      = 100000
      xs     = genList 42 n
      sorted = msort xs
      -- Force the whole spine and payload, and confirm it's actually
      -- sorted, without printing all n numbers.
      (total, isSorted, _) =
        foldl' (\ (!acc, !ok, !prev) x -> (acc + x, ok && x >= prev, x))
               (0 :: Int, True, minBound :: Int) sorted
  print (total, isSorted)
