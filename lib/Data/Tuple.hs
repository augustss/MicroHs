-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Tuple(
  module Data.Tuple,
  ) where
import qualified Prelude()              -- do not import Prelude
import Control.Applicative
import Data.Bool
import Data.Bounded
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int.Int
import Data.Monoid.Internal
import Data.Records
import Data.Ord
import {-# SOURCE #-} Data.Typeable
import Text.Show

--data (a,b) = (a,b)  -- all tuples are built in
--data (a,b,c) = (a,b,c)
-- etc

data Solo a = MkSolo a

getSolo :: Solo a -> a
getSolo (MkSolo a) = a

fst :: forall a b . (a, b) -> a
fst (a, _) = a

snd :: forall a b . (a, b) -> b
snd (_, b) = b

curry :: forall a b c . ((a, b) -> c) -> (a -> b -> c)
curry f a b = f (a, b)

uncurry :: forall a b c . (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b  -- XXX not lazy

swap :: forall a b . (a, b) -> (b, a)
swap (a, b) = (b, a)

-----------------------------------

deriving instance Eq ()
deriving instance Eq a => Eq (Solo a)
deriving instance (Eq  a, Eq  b) => Eq  (a, b)
deriving instance (Eq  a, Eq  b, Eq  c) => Eq  (a, b, c)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d) => Eq  (a, b, c, d)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d, Eq  e) => Eq  (a, b, c, d, e)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)
               => Eq (a, b, c, d, e, f)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)
               => Eq (a, b, c, d, e, f, g)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h)
               => Eq (a, b, c, d, e, f, g, h)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i)
               => Eq (a, b, c, d, e, f, g, h, i)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j)
               => Eq (a, b, c, d, e, f, g, h, i, j)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k)
               => Eq (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

-----------------------------------

deriving instance Ord ()
deriving instance Ord a => Ord (Solo a)
deriving instance (Ord a, Ord b) => Ord (a, b)
deriving instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
deriving instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)
               => Ord (a, b, c, d, e, f)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g)
               => Ord (a, b, c, d, e, f, g)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h)
               => Ord (a, b, c, d, e, f, g, h)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i)
               => Ord (a, b, c, d, e, f, g, h, i)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j)
               => Ord (a, b, c, d, e, f, g, h, i, j)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k)
               => Ord (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

-----------------------------------

instance Show () where
  showsPrec _ () = showString "()"

instance Show a => Show (Solo a) where
  showsPrec p (MkSolo a) = showParen (p > 10) (showString "MkSolo " . showsPrec 11 a)

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a,b) s = showTuple [shows a, shows b] s

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a,b,c) s = showTuple [shows a, shows b, shows c] s

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a,b,c,d) s = showTuple [shows a, shows b, shows c, shows d] s

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a,b,c,d,e) s = showTuple [shows a, shows b, shows c, shows d, shows e] s

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where
  showsPrec _ (a,b,c,d,e,f) s = showTuple [shows a, shows b, shows c, shows d, shows e, shows f] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
        => Show (a,b,c,d,e,f,g) where
  showsPrec _ (a,b,c,d,e,f,g) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
         => Show (a,b,c,d,e,f,g,h) where
  showsPrec _ (a,b,c,d,e,f,g,h) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i)
         => Show (a,b,c,d,e,f,g,h,i) where
  showsPrec _ (a,b,c,d,e,f,g,h,i) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j)
         => Show (a,b,c,d,e,f,g,h,i,j) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k)
         => Show (a,b,c,d,e,f,g,h,i,j,k) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l, Show m)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l, shows m] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l, Show m, Show n)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l, shows m, shows n] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l, Show m, Show n, Show o)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) s
        = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l, shows m, shows n, shows o] s

showTuple :: [ShowS] -> ShowS
showTuple ss = showChar '('
             . foldr1 (\s r -> s . showChar ',' . r) ss
             . showChar ')'
  where
    -- redefine foldr1 to avoid import cycle
    foldr1 _ [x] = x
    foldr1 f (x : xs) = f x (foldr1 f xs)

-----------------------------------

deriving instance Bounded ()
deriving instance (Bounded a) => Bounded (Solo a)
deriving instance (Bounded a, Bounded b) => Bounded (a, b)
deriving instance (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a, b, c, d)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) => Bounded (a, b, c, d, e)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f) => Bounded (a, b, c, d, e, f)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g) => Bounded (a, b, c, d, e, f, g)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h) => Bounded (a, b, c, d, e, f, g, h)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i) => Bounded (a, b, c, d, e, f, g, h, i)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j) => Bounded (a, b, c, d, e, f, g, h, i, j)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k) => Bounded (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l) => Bounded (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m) => Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n) => Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o) => Bounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

-----------------------------------

instance Semigroup () where
  _ <> _ = ()

instance Semigroup a => Semigroup (Solo a) where
  MkSolo a <> MkSolo b = MkSolo (a <> b)

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (a', b') = (a <> a', b <> b')

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a, b, c) <> (a', b', c') = (a <> a', b <> b', c <> c')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a, b, c, d) where
  (a, b, c, d) <> (a', b', c', d') = (a <> a', b <> b', c <> c', d <> d')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (a, b, c, d, e) where
  (a, b, c, d, e) <> (a', b', c', d', e') = (a <> a', b <> b', c <> c', d <> d', e <> e')

-----------------------------------

instance Monoid () where
  mempty = ()

instance Monoid a => Monoid (Solo a) where
  mempty = MkSolo mempty

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  mempty = (mempty, mempty, mempty)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d) where
  mempty = (mempty, mempty, mempty, mempty)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (a, b, c, d, e) where
  mempty = (mempty, mempty, mempty, mempty, mempty)


-----------------------------------
-- Virtual fields for tuples.

instance HasField "_1" (a, b) a where getField _ (a, b) = a
instance SetField "_1" (a, b) a where setField _ (a, b) = \ a -> (a, b)
instance HasField "_2" (a, b) b where getField _ (a, b) = b
instance SetField "_2" (a, b) b where setField _ (a, b) = \ b -> (a, b)

instance HasField "_1" (a, b, c) a where getField _ (a, b, c) = a
instance SetField "_1" (a, b, c) a where setField _ (a, b, c) = \ a -> (a, b, c)
instance HasField "_2" (a, b, c) b where getField _ (a, b, c) = b
instance SetField "_2" (a, b, c) b where setField _ (a, b, c) = \ b -> (a, b, c)
instance HasField "_3" (a, b, c) c where getField _ (a, b, c) = c
instance SetField "_3" (a, b, c) c where setField _ (a, b, c) = \ c -> (a, b, c)

instance HasField "_1" (a, b, c, d) a where getField _ (a, b, c, d) = a
instance SetField "_1" (a, b, c, d) a where setField _ (a, b, c, d) = \ a -> (a, b, c, d)
instance HasField "_2" (a, b, c, d) b where getField _ (a, b, c, d) = b
instance SetField "_2" (a, b, c, d) b where setField _ (a, b, c, d) = \ b -> (a, b, c, d)
instance HasField "_3" (a, b, c, d) c where getField _ (a, b, c, d) = c
instance SetField "_3" (a, b, c, d) c where setField _ (a, b, c, d) = \ c -> (a, b, c, d)
instance HasField "_4" (a, b, c, d) d where getField _ (a, b, c, d) = d
instance SetField "_4" (a, b, c, d) d where setField _ (a, b, c, d) = \ d -> (a, b, c, d)

-----------------------------------

-- | @since base-4.15
instance Functor Solo where
  fmap f (MkSolo a) = MkSolo (f a)

  -- Being strict in the `Solo` argument here seems most consistent
  -- with the concept behind `Solo`: always strict in the wrapper and lazy
  -- in the contents.
  x <$ MkSolo _ = MkSolo x

-- | @since base-4.15
instance Applicative Solo where
  pure = MkSolo
