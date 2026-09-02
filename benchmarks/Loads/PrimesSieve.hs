module Loads.PrimesSieve(main) where

-- The classic (deliberately naive) trial-division sieve built from nested
-- lazy list filters. Heavy on list-cell allocation rather than raw arithmetic.
primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `rem` p /= 0]
    sieve []       = []

main :: IO ()
main = do
  let n = 3000
  print (primes !! (n - 1))
