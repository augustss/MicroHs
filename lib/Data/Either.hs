-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Either(module Data.Either) where
import Primitives
import Data.Bool
import Data.Eq
import Data.Function
import Data.Int
import Data.Ord
import Text.Show

data Either a b = Left a | Right b

instance forall a b . (Eq a, Eq b) => Eq (Either a b) where
  Left  a == Left  a'  =  a == a'
  Right b == Right b'  =  b == b'
  _       == _         =  False

either :: forall a b r . (a -> r) -> (b -> r) -> Either a b -> r
either f _ (Left  a) = f a
either _ f (Right b) = f b

instance forall a b . (Show a, Show b) => Show (Either a b) where
  showsPrec p (Left  a) = showParen (p>=appPrec1) (showString "Left "  . showsPrec appPrec1 a)
  showsPrec p (Right b) = showParen (p>=appPrec1) (showString "Right " . showsPrec appPrec1 b)
