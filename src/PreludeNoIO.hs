module PreludeNoIO(module Prelude) where
import Prelude hiding (Monad(..), MonadFail(..), Applicative(..), Functor(..), (<$>), showString)
