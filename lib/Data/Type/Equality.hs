module Data.Type.Equality(module Data.Type.Equality) where
import Prelude

data a :~: b = (a ~ b) => Refl

-- BUG
--instance forall a b . Eq (a :~: b) where
--  Refl == Refl  =  True

-- BUG
--instance forall a b . Show (a :~: b) where
--  show Refl = "Refl"

sym :: forall a b . (a :~: b) -> (b :~: a)
sym Refl = Refl

trans :: forall a b c . (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl

castWith :: forall a b . (a :~: b) -> a -> b
castWith Refl x = x

-- Problem: :~: is not polykinded
--apply :: forall (f :: Type -> Type) (g :: Type -> Type) a b . (f :~: g) -> (a :~: b) -> (f a :~: g b)
--apply Refl Refl = Refl
