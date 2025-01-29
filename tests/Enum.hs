module Enum(main) where

data T = A | B | C
  deriving (Bounded, Enum, Show)

main :: IO ()
main = do
  print $ succ A
  print $ succ B
  print $ pred B
  print $ pred C
  print $ map (toEnum @T) [0, 1, 2]
  print $ map fromEnum [A, B, C]
  print [A ..]
  print [A, C ..]
  print [C, A ..]
  print [A, B .. C]
  print [C, B .. A]
  print ([minBound .. maxBound] :: [T])
