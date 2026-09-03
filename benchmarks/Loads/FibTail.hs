module Loads.FibTail(main) where

-- Strictly tail-recursive Fibonacci via an accumulator pair, with results
-- kept small by a modulus.
modulus :: Int
modulus = 1000000007

fib :: Int -> Int -> Int -> Int
fib !a !_ 0 = a
fib !a !b n = fib b ((a + b) `rem` modulus) (n - 1)

main :: IO ()
main = do
  let n = 20000000
  print (fib 0 1 n)
