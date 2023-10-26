module Data.Identity(Data.Identity) where
import Primitives
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Identity a = Identity a

instance Functor Identity where
  fmap :: forall a b . (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure :: forall a . a -> Identity a
  pure a = Identity a
  (<*>) :: forall a b . Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  (>>=) :: forall a b . Identity a -> (a -> Identity b) -> Identity b
  Identity a >>= f = f a
