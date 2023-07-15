-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module PreludeNoIO(module Prelude) where
import Prelude hiding (Monad(..), MonadFail(..), Applicative(..), Functor(..), (<$>), showString)
