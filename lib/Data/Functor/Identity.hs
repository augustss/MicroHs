-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Functor.Identity(Identity(..), runIdentity) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.Ord
import Text.Show

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

runIdentity :: forall a . Identity a -> a
runIdentity (Identity a) = a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a
