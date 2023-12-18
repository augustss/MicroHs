module Control.Applicative(module Control.Applicative) where
import Primitives  -- for fixity
import Data.Functor
import Data.Function

infixl 4 <*>
infixl 4 *>
infixl 4 <*

class Functor f => Applicative (f :: Type -> Type) where
  pure  :: forall a . a -> f a
  (<*>) :: forall a b . f (a -> b) -> f a -> f b
  (*>)  :: forall a b . f a -> f b -> f b
  (<*)  :: forall a b . f a -> f b -> f a
  a1 *> a2 = (id <$ a1) <*> a2
  a1 <* a2 = (const <$> a1) <*> a2

liftA2 :: forall (f :: Type -> Type) a b c . Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftA3 :: forall (f :: Type -> Type) a b c d . Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c
