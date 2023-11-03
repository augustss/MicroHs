module Floating(main) where
import Prelude

main :: IO ()
main = do
  print $ log (1000::Double)
  print $ cos (pi::Double)
  print $ sqrt (4::Double)
