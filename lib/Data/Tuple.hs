-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Tuple(
  module Data.Tuple,
  ()(..)
  ) where
import Prelude()              -- do not import Prelude
import Primitives  -- for ()
import Data.Bool
import Data.Bounded
import Data.Eq
import Data.Function
import Data.Int
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Text.Show

--data (a,b) = (a,b)  -- all tuples are built in
--data (a,b,c) = (a,b,c)
-- etc

data Solo a = MkSolo a
  deriving (Eq, Ord)

fst :: forall a b . (a, b) -> a
fst (a, _) = a

snd :: forall a b . (a, b) -> b
snd (_, b) = b

-----------------------------------

instance Eq () where
  () == ()  =  True

instance forall a b . (Eq a, Eq b) => Eq (a, b) where
  (a1, b1) == (a2, b2)  =  a1 == a2 && b1 == b2

instance forall a b c . (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (a1, b1, c1) == (a2, b2, c2)  =  a1 == a2 && b1 == b2 && c1 == c2

instance forall a b c d . (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (a1, b1, c1, d1) == (a2, b2, c2, d2)  =  a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

-----------------------------------

instance Ord () where
  () `compare` ()  =  EQ

instance forall a b . (Ord a, Ord b) => Ord (a, b) where
  (a1, b1) `compare` (a2, b2)  =  a1 `compare` a2 <> b1 `compare` b2

instance forall a b c . (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  (a1, b1, c1) `compare` (a2, b2, c2)  =  a1 `compare` a2 <> b1 `compare` b2 <> c1 `compare` c2

instance forall a b c d . (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  (a1, b1, c1, d1) `compare` (a2, b2, c2, d2)  =  a1 `compare` a2 <> b1 `compare` b2 <> c1 `compare` c2 <> d1 `compare` d2

-----------------------------------

instance Show () where
  showsPrec _ () = showString "()"

instance forall a . Show a => Show (Solo a) where
  showsPrec p (MkSolo a) = showParen (p > 10) (showString "MkSolo " . showsPrec 11 a)

instance forall a b . (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) = showParen True (showsPrec 0 a . showString "," . showsPrec 0 b)

instance forall a b c . (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a, b, c) = showParen True (showsPrec 0 a . showString "," . showsPrec 0 b . showString "," . showsPrec 0 c)

instance forall a b c d . (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a, b, c, d) = showParen True (showsPrec 0 a . showString "," . showsPrec 0 b . showString "," . showsPrec 0 c .
                                             showString "," . showsPrec 0 d)

-----------------------------------

instance Bounded () where
  minBound = ()
  maxBound = ()

instance forall a . (Bounded a) => Bounded (Solo a) where
  minBound = MkSolo minBound
  maxBound = MkSolo maxBound

instance forall a b . (Bounded a, Bounded b) => Bounded (a, b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

instance forall a b c . (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c) where
  minBound = (minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound)

instance forall a b c d . (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a, b, c, d) where
  minBound = (minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound)

-----------------------------------

instance Semigroup () where
  _ <> _ = ()

instance forall a . Semigroup a => Semigroup (Solo a) where
  MkSolo a <> MkSolo b = MkSolo (a <> b)

instance forall a b . (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (a', b') = (a <> a', b <> b')

instance forall a b c . (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a, b, c) <> (a', b', c') = (a <> a', b <> b', c <> c')

instance forall a b c d . (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a, b, c, d) where
  (a, b, c, d) <> (a', b', c', d') = (a <> a', b <> b', c <> c', d <> d')

-----------------------------------

instance Monoid () where
  mempty = ()

instance forall a . Monoid a => Monoid (Solo a) where
  mempty = MkSolo mempty

instance forall a b . (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance forall a b c . (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  mempty = (mempty, mempty, mempty)

instance forall a b c d . (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d) where
  mempty = (mempty, mempty, mempty, mempty)
