-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Error(module Control.Error) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Char_Type

error :: forall a . String -> a
error = primError

undefined :: forall a . a
undefined = error "undefined"
