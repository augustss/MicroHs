module Nfib where
import Prelude
import IO

nfib n =
  case n < 2 of
    False -> nfib (n - 1) + nfib (n - 2) + 1
    True  -> 1

main = print (nfib 38)

-- Typical nfib/s is 10M
