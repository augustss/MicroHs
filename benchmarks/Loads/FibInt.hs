module Loads.FibInt(main) where

-- Same naive doubly-recursive Fibonacci as Loads.FibInteger, but on the
-- fixed-width Int type instead of Integer
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  let n = 33
  print (fib n)
