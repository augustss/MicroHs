module Control.WrappedMonad where
import qualified Prelude()
import Data.Records

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }

newtype WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }
