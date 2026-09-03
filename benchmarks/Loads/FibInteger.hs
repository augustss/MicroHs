module Loads.FibInteger(main) where

-- Naive doubly-recursive Fibonacci on Integer. Exponential call count with
-- tiny-magnitude arithmetic, mostly stresses call/reduction
-- throughput
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  let n = 33
  print (fib n)
