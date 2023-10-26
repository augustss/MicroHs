module Control.Applicative(module Control.Applicative) where
import Primitives  -- for fixity
import Data.Functor

infixl 4 <*>

class Functor f => Applicative (f :: Type -> Type) where
  pure  :: forall a . a -> f a
  (<*>) :: forall a b . f (a -> b) -> f a -> f b
