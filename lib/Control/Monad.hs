module Control.Monad(module Control.Monad) where
import Primitives  -- for fixity
import Control.Applicative
import Control.Error
import Data.Bool
import Data.Char
import Data.Function
import Data.List

infixl 1 >>
infixl 1 >>=

class (Applicative m) => Monad (m :: Type -> Type) where
  (>>=)  :: forall a b . m a -> (a -> m b) -> m b
  (>>)   :: forall a b . m a -> m b -> m b
  ma >> mb = ma >>= \ _ -> mb

  -- Maybe remove this
  return :: forall a . a -> m a
  return = pure

ap :: forall (m :: Type -> Type) a b . Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')

class Monad m => MonadFail (m :: Type -> Type) where
  fail :: forall a . String -> m a
  fail = error

mapM :: forall (m :: Type -> Type) a b . Monad m => (a -> m b) -> [a] -> m [b]
mapM f =
  let
    rec arg =
      case arg of
        [] -> return []
        a : as -> do
          b <- f a
          bs <- rec as
          return (b : bs)
  in rec

mapM_ :: forall (m :: Type -> Type) a b . Monad m => (a -> m b) -> [a] -> m ()
mapM_ f =
  let
    rec arg =
      case arg of
        [] -> return ()
        a : as -> do
          _ <- f a
          rec as
  in rec

when :: forall (m :: Type -> Type) . Monad m => Bool -> m () -> m ()
when False _ = return ()
when True ma = ma

sequence :: forall (m :: Type -> Type) a . Monad m => [m a] -> m [a]
sequence = mapM id
