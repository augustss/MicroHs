-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Functor.Identity(Identity(..)) where
import Primitives
import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.Int
import Data.Ord
import Text.Show

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a

instance forall a . (Show a) => Show (Identity a) where
  showsPrec p (Identity a) = showParen (p >= 11) (showString "Identity " . showsPrec 11 a)
