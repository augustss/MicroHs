-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Error(module Control.Error) where
import Primitives

error :: forall a . [Char] -> a
error = primError

undefined :: forall a . a
undefined = error "undefined"
