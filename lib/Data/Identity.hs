module Data.Identity(Data.Identity) where
import Primitives
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a
