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

class (Applicative m) => Monad m where
  (>>=)  :: forall a b . m a -> (a -> m b) -> m b
  (>>)   :: forall a b . m a -> m b -> m b
  ma >> mb = ma >>= \ _ -> mb

  -- Maybe remove this
  return :: forall a . a -> m a
  return = pure

ap :: forall m a b . Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')

class Monad m => MonadFail m where
  fail :: forall a . String -> m a
  fail = error

mapM :: forall m a b . Monad m => (a -> m b) -> [a] -> m [b]
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

forM :: forall m a b . Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

mapM_ :: forall m a b . Monad m => (a -> m b) -> [a] -> m ()
mapM_ f =
  let
    rec arg =
      case arg of
        [] -> return ()
        a : as -> do
          _ <- f a
          rec as
  in rec

forM_ :: forall m a b . Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

when :: forall m . Monad m => Bool -> m () -> m ()
when False _ = return ()
when True ma = ma

sequence :: forall m a . Monad m => [m a] -> m [a]
sequence = mapM id

sequence_ :: forall m a . Monad m => [m a] -> m ()
sequence_ = mapM_ id

(=<<) :: forall m a b . Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

infixr 1 <=<
(<=<) :: forall m a b c . Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \ a -> do
  b <- g a
  f b

infixr 1 >=>
(>=>) :: forall m a b c . Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = flip (<=<)

filterM :: forall m a . Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
  b <- p x
  ts <- filterM p xs
  return $ if b then x : ts else ts

partitionM :: forall m a . Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
  b <- p x
  (ts,fs) <- partitionM p xs
  return $ if b then (x:ts, fs) else (ts, x:fs)

instance forall a . Functor ((->) a) where
  fmap = (.)

instance forall a . Applicative ((->) a) where
  pure = const
  f <*> g = \ a -> f a (g a)

instance forall a . Monad ((->) a) where
  x >>= y = \ z -> y (x z) z

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

class (Monad m) => MonadPlus m where
  mzero :: forall a . m a
  mplus :: forall a . m a -> m a -> m a
