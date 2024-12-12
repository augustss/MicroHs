module Mhs.Builtin(
  module Control.Monad,
  module Data.Bool,
  module Data.Enum,
  module Data.Eq,
  module Data.Fractional,
  module Data.Function,
  module Data.Ord,
  module Data.Num,
  module Data.Records,
  module Data.Typeable,
  module Data.Semigroup,
  module Data.String,
  module Text.Show,
  P0(..), P1(..), P2(..), P3(..), P4(..),
  PC0(..), PC1(..), PC2(..), PC3(..), PC4(..),
  ) where
import Prelude()
import Control.Error(error)
import Control.Monad(Monad(..))
import Control.Monad.Fail(MonadFail(..))
import Data.Bool((&&), Bool(..))
import Data.Enum(Enum(enumFrom, enumFromThen, enumFromTo, enumFromThenTo))
import Data.Eq(Eq(..))
import Data.Fractional(Fractional(fromRational))
import Data.Function((.))
import Data.Ord(Ord(..), Ordering(..))
import Data.Num(Num(fromInteger))
import Data.Proxy(Proxy(..))
import Data.Semigroup(Semigroup(..))
import Data.String(IsString(..))
import Data.Records(HasField(..), SetField(..), composeSet)
import {-# SOURCE #-} Data.Typeable(Typeable(..), mkTyConApp, mkTyCon)
import Text.Show(Show(..), showString, showParen)

-- These types are used as return values for pattern synonym matching functions.
-- The number indicates the number of parameters to the synonym.
-- Nx is the non-match, Mx is the match, carrying the matched values.
data P0 = N0 | M0
data P1 a1 = N1 | M1 a1
data P2 a1 a2 = N2 | M2 a1 a2
data P3 a1 a2 a3 = N3 | M3 a1 a2 a3
data P4 a1 a2 a3 a4 = N4 | M4 a1 a2 a3 a4

-- For synonyms with a constructor context
data PC0 ctx = NC0 | ctx => MC0
data PC1 ctx a1 = NC1 | ctx => MC1 a1
data PC2 ctx a1 a2 = NC2 | ctx => MC2 a1 a2
data PC3 ctx a1 a2 a3 = NC3 | ctx => MC3 a1 a2 a3
data PC4 ctx a1 a2 a3 a4 = NC4 | ctx => MC4 a1 a2 a3 a4
