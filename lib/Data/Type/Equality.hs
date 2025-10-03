module Data.Type.Equality (
  --type (~~),
  (:~:)(..),
  (:~~:)(..),
  sym, trans, castWith, gcastWith, apply, inner, outer,
  TestEquality(..),
  ) where
import qualified Prelude(); import MiniPrelude
import Data.Bounded
import {-# SOURCE #-} Data.Typeable
import Text.Read(Read)
import Mhs.Builtin

infix 4 :~:, :~~:

type (:~:) :: forall k . k -> k -> Type
data a :~: b where
  Refl :: a :~: a

sym :: (a :~: b) -> (b :~: a)
sym Refl = Refl

trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl

castWith :: (a :~: b) -> a -> b
castWith Refl x = x

gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x

apply :: (f :~: g) -> (a :~: b) -> (f a :~: g b)
apply Refl Refl = Refl

inner :: (f a :~: g b) -> (a :~: b)
inner Refl = Refl

outer :: (f a :~: g b) -> (f :~: g)
outer Refl = Refl

deriving instance Eq   (a :~: b)
deriving instance Show (a :~: b)
deriving instance Ord  (a :~: b)
deriving instance a ~ b => Read (a :~: b)

instance a ~ b => Enum (a :~: b) where
  toEnum 0 = Refl
  toEnum _ = error "Data.Type.Equality.toEnum: bad argument"

  fromEnum Refl = 0

deriving instance a ~ b => Bounded (a :~: b)

type (:~~:) :: forall k1 k2 . k1 -> k2 -> Type
data a :~~: b where
   HRefl :: a :~~: a

deriving instance Eq   (a :~~: b)
deriving instance Show (a :~~: b)
deriving instance Ord  (a :~~: b)
{- XXX no ~~
deriving instance a ~~ b => Read (a :~~: b)
instance a ~~ b => Enum (a :~~: b) where
  toEnum 0 = HRefl
  toEnum _ = error "Data.Type.Equality.toEnum: bad argument"

  fromEnum HRefl = 0

deriving instance a ~~ b => Bounded (a :~~: b)
-}

class TestEquality f where
  testEquality :: f a -> f b -> Maybe (a :~: b)

instance TestEquality ((:~:) a) where
  testEquality Refl Refl = Just Refl

instance TestEquality ((:~~:) a) where
  testEquality HRefl HRefl = Just Refl

{-
infix 4 ==

type (==) :: k -> k -> Bool
type family a == b where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False
-}
