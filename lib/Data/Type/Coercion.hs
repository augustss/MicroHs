module Data.Type.Coercion
  ( Coercion(..)
  , coerceWith
  , gcoerceWith
  , sym
--  , trans
  , repr
  , TestCoercion(..)
  ) where
import Data.Coerce
import qualified Data.Type.Equality as Eq

data Coercion a b where
  Coercion :: Coercible a b => Coercion a b

coerceWith :: Coercion a b -> a -> b
coerceWith Coercion x = coerce x

gcoerceWith :: Coercion a b -> (Coercible a b => r) -> r
gcoerceWith Coercion x = x

sym :: Coercion a b -> Coercion b a
sym Coercion = Coercion

--XXX BUG: mhs does not take the transitive closure.
--trans :: Coercion a b -> Coercion b c -> Coercion a c
--trans Coercion Coercion = Coercion

repr :: (a Eq.:~: b) -> Coercion a b
repr Eq.Refl = Coercion

deriving instance Eq   (Coercion a b)
deriving instance Show (Coercion a b)
deriving instance Ord  (Coercion a b)
deriving instance Coercible a b => Read (Coercion a b)
instance Coercible a b => Enum (Coercion a b) where
  toEnum 0 = Coercion
  toEnum _ = error "Data.Type.Coercion.toEnum: bad argument"

  fromEnum Coercion = 0

deriving instance Coercible a b => Bounded (Coercion a b)

class TestCoercion f where
  testCoercion :: f a -> f b -> Maybe (Coercion a b)

instance TestCoercion ((Eq.:~:) a) where
  testCoercion Eq.Refl Eq.Refl = Just Coercion

instance TestCoercion ((Eq.:~~:) a) where
  testCoercion Eq.HRefl Eq.HRefl = Just Coercion

--XXX BUG
--instance TestCoercion (Coercion a) where
--  testCoercion Coercion Coercion = Just Coercion
