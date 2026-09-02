module Loads.NFib(main) where

-- Classic combinator-reduction benchmark: deeply recursive, tiny-int
-- arithmetic, no data structure allocation.
nfib :: Int -> Int
nfib n =
  case n < 2 of
    True  -> 1
    False -> nfib (n - 1) + nfib (n - 2) + 1

main :: IO ()
main = do
  let n = 33
  print (nfib n)
