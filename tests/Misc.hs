module Misc(module Misc) where
import Prelude

first :: (a, b) -> a
first ab =
  let { (a, b) = ab }
  in  a

main = do
  print $ first (10,20)
