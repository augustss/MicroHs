module Floating(main) where

main :: IO ()
main = do
  print $ logBase 10 (1000::Double)
  print $ cos (pi::Double)
  print $ sqrt (4::Double)

  print $ logBase 10 (1000::Float)
  print $ cos (pi::Float)
  print $ sqrt (4::Float)
