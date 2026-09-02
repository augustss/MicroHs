module Loads.ListFusion(main) where

-- Builds a list purely to immediately consume it: [1..n] is produced,
-- filtered, mapped, and summed. With foldr/build-style deforestation this
-- compiles down to a single allocation-free loop; without it, three full
-- n-cons-cell lists get materialized and garbage-collected.
main :: IO ()
main = do
  let n = 3000000
  print (sum (map (+ 1) (filter even [1 .. n])))
