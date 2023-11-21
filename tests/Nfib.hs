module Nfib(main) where
import Prelude

nfib :: Int -> Int
nfib n =
  case n < 2 of
    False -> nfib (n - 1) + nfib (n - 2) + 1
    True  -> 1

main :: IO ()
main = print (nfib 44)

-- Typical nfib/s is 10M
-- mhs
-- 126491971 / 15.68 = 8.07M
-- ghc
-- 126491971 / 0.236 = 535M
