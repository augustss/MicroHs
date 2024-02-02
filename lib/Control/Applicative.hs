module Control.Applicative(
  Applicative(..),
  (<$>), (<$), (<**>),
  liftA, liftA3,
  ) where
import Primitives  -- for fixity
import Data.Functor
import Data.Function

infixl 4 <*>
infixl 4 *>
infixl 4 <*
infixl 4 <**>

class Functor f => Applicative f where
  pure        :: forall a . a -> f a
  (<*>)       :: forall a b . f (a -> b) -> f a -> f b
  (*>)        :: forall a b . f a -> f b -> f b
  (<*)        :: forall a b . f a -> f b -> f a
  liftA2      :: forall a b c . (a -> b -> c) -> f a -> f b -> f c
  (<*>)        = liftA2 id
  a1 *> a2     = (id <$ a1) <*> a2
  a1 <* a2     = const <$> a1 <*> a2
  liftA2 f a b = f <$> a <*> b

liftA :: forall f a b . Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA3 :: forall f a b c d . Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

(<**>) :: forall f a b . Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\ a f -> f a)
