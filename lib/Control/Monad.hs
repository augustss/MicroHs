module Control.Monad(module Control.Monad) where
import Primitives  -- for fixity
import Control.Applicative

infixl 1 >>
infixl 1 >>=

class (Applicative m) => Monad (m :: Type -> Type) where
  (>>=)  :: forall a b . m a -> (a -> m b) -> m b
  (>>)   :: forall a b . m a -> m b -> m b

--  ma >> mb = ma >>= \ _ -> mb

return :: forall (m :: Type -> Type) a . Monad m => a -> m a
return = pure
