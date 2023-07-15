-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Error(module Control.Error) where
import Primitives

--error :: String -> a
error = primError

undefined :: a
undefined = error "undefined"
