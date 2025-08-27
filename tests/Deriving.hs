module Deriving(main) where

import Data.Ix

data T a b c = A a | B b | C a Int | D
  deriving (Eq, Ord, Show)

data Rec a = R { x :: a, y :: Int }
  deriving Show

newtype Alt f a = Alt (f a)
  deriving Show

data E = X | Y | Z
  deriving (Enum, Bounded, Show, Eq, Ord, Ix)

data F = MkF Bool Int deriving (Show, Eq, Ord, Ix)

-- Not yet
-- data F a = F0 | F1 a | F2 (a,a) | F3 Int | F4 a Int | F5 (Int -> a)
--   deriving Functor

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

  print (A 'a' :: T Char () ())
  print (B False :: T () Bool ())
  print (C 'a' 1 :: T Char () ())
  print (D :: T () () ())
  print (A (A 'a') :: T (T Char () ()) () ())

  print $ R{ x='a', y=10 }
  print $ R{ x=R{x='b',y=11}, y=10 }

  print $ Alt [True]

  print $ fromEnum Y
  print (minBound :: E, maxBound :: E)
  -- Ix E
  print $ range (X, X)
  print $ range (X, Y)
  print $ range (X, Z)
  print $ range (Y, X)
  print $ range (Y, Y)
  print $ range (Y, Z)
  print $ range (Z, X)
  print $ range (Z, Y)
  print $ range (Z, Z)
  print $ unsafeIndex (X, X) X
  print $ unsafeIndex (X, Y) X
  print $ unsafeIndex (X, Z) X
  print $ unsafeIndex (Y, X) X
  print $ unsafeIndex (Y, Y) X
  print $ unsafeIndex (Y, Z) X
  print $ unsafeIndex (Z, X) X
  print $ unsafeIndex (Z, Y) X
  print $ unsafeIndex (Z, Z) X
  print $ unsafeIndex (X, X) Y
  print $ unsafeIndex (X, Y) Y
  print $ unsafeIndex (X, Z) Y
  print $ unsafeIndex (Y, X) Y
  print $ unsafeIndex (Y, Y) Y
  print $ unsafeIndex (Y, Z) Y
  print $ unsafeIndex (Z, X) Y
  print $ unsafeIndex (Z, Y) Y
  print $ unsafeIndex (Z, Z) Y
  print $ unsafeIndex (X, X) Z
  print $ unsafeIndex (X, Y) Z
  print $ unsafeIndex (X, Z) Z
  print $ unsafeIndex (Y, X) Z
  print $ unsafeIndex (Y, Y) Z
  print $ unsafeIndex (Y, Z) Z
  print $ unsafeIndex (Z, X) Z
  print $ unsafeIndex (Z, Y) Z
  print $ unsafeIndex (Z, Z) Z
  print $ inRange (X, X) X
  print $ inRange (X, Y) X
  print $ inRange (X, Z) X
  print $ inRange (Y, X) X
  print $ inRange (Y, Y) X
  print $ inRange (Y, Z) X
  print $ inRange (Z, X) X
  print $ inRange (Z, Y) X
  print $ inRange (Z, Z) X
  print $ inRange (X, X) Y
  print $ inRange (X, Y) Y
  print $ inRange (X, Z) Y
  print $ inRange (Y, X) Y
  print $ inRange (Y, Y) Y
  print $ inRange (Y, Z) Y
  print $ inRange (Z, X) Y
  print $ inRange (Z, Y) Y
  print $ inRange (Z, Z) Y
  print $ inRange (X, X) Z
  print $ inRange (X, Y) Z
  print $ inRange (X, Z) Z
  print $ inRange (Y, X) Z
  print $ inRange (Y, Y) Z
  print $ inRange (Y, Z) Z
  print $ inRange (Z, X) Z
  print $ inRange (Z, Y) Z
  print $ inRange (Z, Z) Z

  -- Ix F
  let r = (MkF False 2, MkF True 5)
  print $ range r
  print $ unsafeIndex r (MkF True 3)
  print $ inRange r (MkF True 3)
