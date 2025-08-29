module Control.Monad.Fix where
import qualified Prelude()
import Control.Monad

type MonadFix :: (Type -> Type) -> Constraint
class (Monad m) => MonadFix m where

_mfix :: MonadFix m => (a -> m a) -> m a
