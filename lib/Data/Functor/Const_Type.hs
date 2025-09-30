-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Functor.Const_Type(Const(..)) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Bool
import Data.Coerce
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int.Int
import Data.Ord
import {-# SOURCE #-} Data.Typeable
import Text.Show

type Const :: forall k . Type -> k -> Type
newtype Const a b = Const { getConst :: a }
  deriving (Eq, Ord, Show)
