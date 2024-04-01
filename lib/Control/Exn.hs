-- Copyright 2024 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Exn(Exn(..), exnToString) where

-- Temporary exception type until we get proper exceptions.

newtype Exn = Exn String

exnToString :: Exn -> String
exnToString (Exn s) = s

