-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Functor.Const(Const(..), getConst) where
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
import Data.Functor.Const_Type
import Data.Integral
import Data.Int.Int
import Data.Monoid.Internal
import Data.Num
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Data.Ord
import Text.Show

deriving instance Functor (Const a)
deriving instance Show a => Show (Const a b)

deriving instance Enum a => Enum (Const a b)
deriving instance Eq a => Eq (Const a b)
deriving instance Floating a => Floating (Const a b)
deriving instance Fractional a => Fractional (Const a b)
deriving instance Integral a => Integral (Const a b)
deriving instance Num a => Num (Const a b)
deriving instance Ord a => Ord (Const a b)
deriving instance Real a => Real (Const a b)
deriving instance RealFloat a => RealFloat (Const a b)
deriving instance RealFrac a => RealFrac (Const a b)

instance forall m . Monoid m => Applicative (Const m) where
  pure _ = Const mempty
  Const a <*> Const b = Const (a `mappend` b)
