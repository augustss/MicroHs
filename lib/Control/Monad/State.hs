{-# LANGUAGE QualifiedDo #-}
module Control.Monad.State(module Control.Monad.State) where
import Prelude

data State s a = S (s -> (a, s))

runState :: State s a -> (s -> (a,s))
runState sa =
  case sa of
    S x -> x

(>>=) :: State s a -> (a -> State s b) -> State s b
(>>=) m k = S $ \ s ->
  case runState m s of
    (a, ss) -> runState (k a) ss

(>>) :: State s a -> State s b -> State s b
(>>) m k = S $ \ s ->
  case runState m s of
    (_, ss) -> runState k ss

return :: a -> State s a
return a = S $ \ s -> (a, s)

fmap :: (a -> b) -> State s a -> State s b
fmap f sa = S $ \ s ->
  case runState sa s of
    (a, ss) -> (f a, ss)

modify :: (s -> s) -> State s ()
modify f = S $ \ s -> ((), f s)

put :: s -> State s ()
put s = S $ \ _ -> ((), s)

get :: State s s
get = S $ \ s -> (s, s)

gets :: (s -> a) -> State s a
gets f = S $ \ s -> (f s, s)

mapM :: (a -> State s b) -> [a] -> State s [b]
mapM f =
  let
    rec arg =
      case arg of
        [] -> Control.Monad.State.return []
        a : as -> Control.Monad.State.do
          b <- f a
          bs <- rec as
          Control.Monad.State.return (b : bs)
  in rec
