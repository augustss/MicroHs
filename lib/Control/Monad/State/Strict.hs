{-# LANGUAGE QualifiedDo #-}
module Control.Monad.State.Strict(
  module Control.Monad.State.Strict,
  module Control.Monad,
  ) where
import Prelude
import Control.Monad
import Control.Monad.Fail

data State s a = S (s -> (a, s))

instance forall s . Functor (State s) where
  fmap f sa = S $ \ s ->
    case runState sa s of
      (a, ss) -> (f a, ss)

instance forall s . Applicative (State s) where
  pure a = S $ \ s -> (a, s)
  (<*>) = ap
  (*>) m k = S $ \ s ->
    case runState m s of
      (_, ss) -> runState k ss

instance forall s . Monad (State s) where
  (>>=) m k = S $ \ s ->
    case runState m s of
      (a, ss) -> runState (k a) ss
  (>>) = (*>)
  return = pure

instance forall s . MonadFail (State s)

runState :: forall s a . State s a -> (s -> (a,s))
runState (S x) = x

evalState :: forall s a . State s a -> (s -> a)
evalState sa = fst . runState sa

{-
(>>=) :: forall s a b . State s a -> (a -> State s b) -> State s b
(>>=) m k = S $ \ s ->
  case runState m s of
    (a, ss) -> runState (k a) ss

(>>) :: forall s a b . State s a -> State s b -> State s b
(>>) m k = S $ \ s ->
  case runState m s of
    (_, ss) -> runState k ss

return :: forall s a . a -> State s a
return a = S $ \ s -> (a, s)

fmap :: forall s a b . (a -> b) -> State s a -> State s b
fmap f sa = S $ \ s ->
  case runState sa s of
    (a, ss) -> (f a, ss)

(<$>) :: forall s a b . (a -> b) -> State s a -> State s b
(<$>) = Control.Monad.State.Strict.fmap

(<*>) :: forall s a b . State s (a -> b) -> State s a -> State s b
(<*>) sf sa = Control.Monad.State.Strict.do
  f <- sf
  a <- sa
  Control.Monad.State.Strict.return (f a)
-}

modify :: forall s . (s -> s) -> State s ()
modify f = S $ \ s -> ((), f s)

put :: forall s . s -> State s ()
put s = S $ \ _ -> ((), s)

get :: forall s . State s s
get = S $ \ s -> (s, s)

gets :: forall s a . (s -> a) -> State s a
gets f = S $ \ s -> (f s, s)

{-
mapM :: forall s a b . (a -> State s b) -> [a] -> State s [b]
mapM f =
  let
    rec arg =
      case arg of
        [] -> Control.Monad.State.Strict.return []
        a : as -> Control.Monad.State.Strict.do
          b <- f a
          bs <- rec as
          Control.Monad.State.Strict.return (b : bs)
  in rec

mapM_ :: forall s a b . (a -> State s b) -> [a] -> State s ()
mapM_ f =
  let
    rec arg =
      case arg of
        [] -> Control.Monad.State.Strict.return ()
        a : as -> Control.Monad.State.Strict.do
          f a
          rec as
  in rec

fail :: forall s a . String -> State s a
fail = error

when :: forall s . Bool -> State s () -> State s ()
when True  s = s
when False _ = Control.Monad.State.Strict.return ()

sequence :: forall s a . [State s a] -> State s [a]
sequence = Control.Monad.State.Strict.mapM id
-}
