module Deriving(main) where
import Prelude

data T a b c = A a | B b | C a Int | D
  deriving (Eq, Ord)

main :: IO ()
main = do
  print $ A 'a' == (A 'a' :: T Char () ())
  print $ A 'a' == (A 'b' :: T Char () ())
  print $ A 'a' == B False
  print $ C 'a' 1 == (C 'a' 1 :: T Char () ())
  print $ C 'a' 1 == (C 'a' 2 :: T Char () ())
  print $ D == (D :: T () () ())

  print $ A 'a' <= (A 'a' :: T Char () ())
  print $ A 'a' <= (A 'b' :: T Char () ())
  print $ A 'b' <= (A 'a' :: T Char () ())
  print $ A 'a' <= B False
  print $ C 'a' 1 <= B False
