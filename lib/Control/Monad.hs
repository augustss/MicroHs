module Control.Monad(module Control.Monad) where
import Primitives  -- for fixity
import Control.Applicative
import Control.Error
import Data.Bool
import Data.Char_Type
import Data.Function
import Data.Functor
import Data.List_Type
--import Data.Maybe

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

(=<<) :: forall (m :: Type -> Type) a b . Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

infixr 1 <=<
(<=<) :: forall (m :: Type -> Type) a b c . Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \ a -> do
  b <- g a
  f b

infixr 1 >=>
(>=>) :: forall (m :: Type -> Type) a b c . Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = flip (<=<)

{-
-- Same for Maybe
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure a = Just a
  (<*>) = ap

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just a  >>= f = f a
-}
