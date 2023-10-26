module Data.Identity(Data.Identity) where
import Primitives
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Identity a = Identity a

fm :: forall a b . (a -> b) -> Identity a -> Identity b
fm f (Identity a) = Identity (f a)

instance Functor Identity where
  fmap :: forall a b . (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

{-
instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a
-}
