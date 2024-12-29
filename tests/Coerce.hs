module Coerce where
import Data.Coerce

newtype NT = NT Int
  deriving (Show)

newtype List a = List [a]
  deriving (Show)

toInt :: (Coercible a Int) => a -> Int
toInt = coerce

toInts :: (Coercible a Int) => [a] -> [Int]
toInts = coerce

main :: IO ()
main = do
  print (toInt (NT 5))
  print (toInts [NT 1, NT 2] :: [Int])
  print (coerce (List [3::Int, 4]) :: [Int])
  print (coerce (List [NT 5, NT 6]) :: [NT])
  print (coerce (List [NT 7, NT 8]) :: [Int])
