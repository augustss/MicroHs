module Deriving(main) where
import Control.Monad
import Data.Char(chr)
import qualified Data.Foldable as F
import Data.Ix
import Data.Typeable
import Data.Traversable
import Text.Read (readMaybe)

data T a b c = A a | B b | C a Int | D
  deriving (Eq, Ord, Show, Read)

data Rec a = R { x :: a, y :: Int }
  deriving (Show, Read)

newtype Alt f a = Alt (f a)
  deriving Show

data E = X | Y | Z
  deriving (Enum, Bounded, Show, Eq, Ord, Ix, Typeable)

data FF a b = FF a b
  deriving (Show, Functor)

data F a = F0 | F1 a | F2 (a, [a]) | F3 Integer | F4 a Integer | F5 (Integer -> a) | F6 (Maybe [F a]) | F7 (FF Int a) | F8 [(a, a)]
  deriving (Show, Functor)
instance Show (Integer -> Char) where show f = "fcn(100)=" ++ show (f 100)

data G q a = G1 a | G2 (q a)
  deriving (Show, Functor)

data H a = H0 | H1 Int | H2 a | H3 (H a) | H4 (H a) (H a) | H5 [(a, Bool, Maybe a)] | H6 [H a]
  deriving (Foldable)

data U a = U1 | U2 Int | U3 a | U4 a Int (a,a) | U5 [U a]
  deriving (Show, Functor, Foldable, Traversable)

newtype St a = St (Int -> (Int, a))
  deriving (Functor)
instance Applicative St where
  pure x = St $ \ i -> (i, x)
  (<*>) = ap
instance Monad St where
  St sa >>= sk = St $ \ i ->
    case sa i of
      (i', a) ->
        case sk a of
          St f -> f i'
get :: St Int
get = St $ \ i -> (i, i)
put :: Int -> St ()
put i = St $ const (i, ())
num :: a -> St (a, Int)
num a = do
  i <- get
  put (i+1)
  return (a, i)
run :: St a -> a
run (St f) = snd (f 1)

data Pair = MkPair Bool Int deriving (Show, Eq, Ord, Ix)

infixr 5 :^:
data Tree a = Leaf a | Tree a :^: Tree a
  deriving (Show, Read)

infixr 5 `Cons`
data List a = Nil | a `Cons` List a
  deriving (Show, Read)

newtype Op = (:::) () deriving (Read, Show)

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

  print (read "A 42" :: T Int () ())
  print (read "(B (D))" :: T () (T () () ()) ())
  print (read " ( C (((\tTrue\t)))3  ) " :: T Bool () ())
  print (read "D" :: T () () ())

  print $ R{ x='a', y=10 }
  print $ R{ x=R{x='b',y=11}, y=10 }
  print (read "R{x=True,y=12}" :: Rec Bool)
  print (read "R { x = True , y = 12 }" :: Rec Bool)

  print $ Alt [True]

  let fs = [F0, F1 49, F2 (50, [51,52]), F3 5, F4 54 7, F5 fromInteger, F6 (Just [F1 56, F1 57]), F7 (FF 0 58), F8 [(59,60),(61,62)]] :: [F Int]
  print $ map (fmap chr) fs
  let gs = [G1 True, G2 [True, False]]
  print $ map (fmap show) gs

  let h = H6 [H0, H1 1, H2 1, H3 (H2 2), H4 (H2 3) (H2 4), H5 [(6, False, Just 7), (8, False, Nothing)]]
  print $ F.foldr (:) [] h

  let u = U5 [U1, U2 1, U3 True, U4 False 1 (True, False)]
  print $ run (traverse num u)
  

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

  -- Ix Pair
  let r = (MkPair False 2, MkPair True 5)
  print $ range r
  print $ unsafeIndex r (MkPair True 3)
  print $ inRange r (MkPair True 3)

  print (Leaf 1 :^: Leaf 2 :: Tree Int)
  print (read "Leaf 1 :^: Leaf 2" :: Tree Int)
  print (readMaybe "(:^:) (Leaf 1) (Leaf 2)" :: Maybe (Tree Int))

  print (1 `Cons` 2 `Cons` Nil :: List Int)
  print (read "1 `Cons` (2 `Cons` Nil)" :: List Int)
  print (readMaybe "1 `Cons` 2 `Cons` Nil" :: Maybe (List Int))
  print (readMaybe "Cons 1 (Cons 2 Nil)" :: Maybe (List Int))

  print ((:::) ())
  print (read "(:::) ()" :: Op)
  print (readMaybe "::: ()" :: Maybe Op)

  -- Check that they all have Typeable
  print (typeOf (R True 1), X)
