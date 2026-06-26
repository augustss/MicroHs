-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Functor.Identity(Identity(..)) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Coerce
import Data.Enum
import Data.Eq
import Data.Floating
import Data.Fractional
import Data.Function
import Data.Functor
import Data.Integral
import Data.Int.Int
import Data.Monoid.Internal
import Data.Num
import Data.Ord
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Data.Records   -- needed since we don't import Mhs.Builtin
import {-# SOURCE #-} Data.Typeable
import Text.Show

newtype Identity a = Identity { runIdentity :: a }
  deriving stock   ({-Read,-} Show, Functor)
  deriving newtype (Enum, Eq, Floating, Fractional, Integral, Monoid,
                    Num, Ord, Real, RealFloat, RealFrac, Semigroup)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

-- Read instance in Text.Read.Internal
-- Foldable instance in Data.Foldable
-- Traversable instance in Data.Traverse

instance Monad Identity where
  Identity a >>= f = f a
