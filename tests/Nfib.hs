module Nfib(main) where
import Prelude

nfib :: Int -> Int
nfib n =
  case n < 2 of
    False -> nfib (n - 1) + nfib (n - 2) + 1
    True  -> 1

main :: IO ()
main = print (nfib 38)

-- Typical nfib/s is 10M
