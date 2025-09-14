module Control.WrappedMonad where
import qualified Prelude()

newtype WrappedMonad m a = WrapMonad (m a)

newtype WrappedArrow a b c = WrapArrow (a b c)
