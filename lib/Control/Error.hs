-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Error(_error, error, errorWithoutStackTrace, undefined, ErrorCall(..)) where
import qualified Prelude()              -- do not import Prelude
import Data.Char_Type
import Control.Exception.Internal

-- no location of call added
_error :: forall a . String -> a
_error s = throw (ErrorCall s)

-- mhs compiler adds location of call
error :: forall a . String -> a
error = _error

-- mhs compiler adds location of call
undefined :: forall a . a
undefined = error "undefined"

-- GHC compatibility
errorWithoutStackTrace :: forall a . String -> a
errorWithoutStackTrace = _error
