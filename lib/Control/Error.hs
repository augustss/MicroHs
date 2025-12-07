-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Error(
  error, _errorLoc,
  undefined, _undefinedLoc,
  errorWithoutStackTrace,
  ) where
import qualified Prelude()              -- do not import Prelude
import Data.Char_Type
import Control.Exception.Internal

-- mhs compiler adds location of call and rewrites to _errorLoc
error :: forall a . String -> a
error = _errorLoc ""

-- mhs compiler adds location of call and rewrites to _undefinedLoc
undefined :: forall a . a
undefined = _undefinedLoc ""

-- no location of call added
_errorLoc :: forall a . String -> String -> a
_errorLoc l s = throw (ErrorCallWithLocation s l)

-- no location of call added
_undefinedLoc :: forall a . String -> a
_undefinedLoc l = throw (ErrorCallWithLocation "undefined" l)

-- Error without location
errorWithoutStackTrace :: forall a . String -> a
errorWithoutStackTrace = _errorLoc ""
