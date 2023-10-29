module Data.Functor(module Data.Functor) where
import Primitives  -- for fixity
import Data.Function

class Functor (f :: Type -> Type) where
  fmap :: forall a b . (a -> b) -> f a -> f b
  (<$) :: forall a b . a -> f b -> f a
  (<$) =  fmap . const

infixl 4 <$>
(<$>) :: forall (f :: Type -> Type) a b . Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

--void :: forall f a . Functor f => f a -> f ()
--void = fmap (const ())
