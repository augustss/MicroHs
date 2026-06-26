-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Functor.Const_Type(Const(..)) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import {-# SOURCE #-} Data.Typeable

type Const :: forall k . Type -> k -> Type
newtype Const a b = Const { getConst :: a }
