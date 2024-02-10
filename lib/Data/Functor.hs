module Data.Functor(module Data.Functor) where
import Prelude()              -- do not import Prelude
import Primitives  -- for fixity
import Data.Function

class Functor f where
  fmap :: forall a b . (a -> b) -> f a -> f b
  (<$) :: forall a b . a -> f b -> f a
  (<$) =  fmap . const

infixl 4 <$>
(<$>) :: forall f a b . Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

void :: forall f a . Functor f => f a -> f ()
void = fmap (const ())
