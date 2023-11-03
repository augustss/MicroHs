-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Tuple(
  module Data.Tuple,
--Y{-
  ()(..)
--Y-}
  ) where
import Primitives  -- for ()
--Yimport PrimFromInteger
import Data.Bool
import Data.Bounded
import Data.Eq
import Data.Function
import Text.Show

--data (a,b) = (a,b)  -- all tuples are built in
--data (a,b,c) = (a,b,c)
-- etc

fst :: forall a b . (a, b) -> a
fst (a, _) = a

snd :: forall a b . (a, b) -> b
snd (_, b) = b

eqPair :: forall a b . (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a1, b1) (a2, b2) = eqa a1 a2 && eqb b1 b2

instance forall a b . (Eq a, Eq b) => Eq (a, b) where
  (a1, b1) == (a2, b2)  =  a1 == a2 && b1 == b2

instance forall a b c . (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (a1, b1, c1) == (a2, b2, c2)  =  a1 == a2 && b1 == b2 && c1 == c2

instance forall a b c d . (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (a1, b1, c1, d1) == (a2, b2, c2, d2)  =  a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

instance Show () where
  showsPrec _ () = showString "()"

instance forall a b . (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) = showParen True (showsPrec 0 a . showString "," . showsPrec 0 b)

instance forall a b c . (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a, b, c) = showParen True (showsPrec 0 a . showString "," . showsPrec 0 b . showString "," . showsPrec 0 c)

instance forall a b c d . (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a, b, c, d) = showParen True (showsPrec 0 a . showString "," . showsPrec 0 b . showString "," . showsPrec 0 c .
                                             showString "," . showsPrec 0 d)

instance Bounded () where
  minBound = ()
  maxBound = ()

instance forall a b . (Bounded a, Bounded b) => Bounded (a, b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

instance forall a b c . (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c) where
  minBound = (minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound)

instance forall a b c d . (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a, b, c, d) where
  minBound = (minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound)
