module Enum(main) where

data T = A | B | C
  deriving (Bounded, Enum, Show)

main :: IO ()
main = print ([minBound .. maxBound] :: [T])
