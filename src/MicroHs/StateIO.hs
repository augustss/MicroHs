-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-dodgy-imports #-}
-- State monad over IO
module MicroHs.StateIO(
  module MicroHs.StateIO,
  module Control.Applicative,
  module Control.Monad,
  module Data.Functor,
  ) where
import Prelude
import Control.Applicative
import Control.Monad
import Data.Functor --Xhiding(unzip)
--import qualified System.IO as IO
--Ximport qualified CompatIO as IO

data StateIO s a = S (s -> IO (a,s))

runStateIO :: forall s a . StateIO s a -> (s -> IO (a,s))
runStateIO sa =
  case sa of
    S x -> x

{-
execStateIO :: forall s a . StateIO s a -> s -> IO s
execStateIO sa s = IO.do
  as <- runStateIO sa s
  case as of
    (_, ss) -> IO.return ss
-}

instance forall s . Functor (StateIO s) where
  fmap f sa = S $ \ s -> do
    (a, ss) <- runStateIO sa s
    return (f a, ss)

instance forall s . Applicative (StateIO s) where
  pure a = S $ \ s -> return (a, s)
  (<*>) = ap
  (*>) m k = S $ \ s -> do
    (_, ss) <- runStateIO m s
    runStateIO k ss

instance forall s . Monad (StateIO s) where
  (>>=) m k = S $ \ s -> do
    (a, ss) <- runStateIO m s
    runStateIO (k a) ss
  (>>) = (*>)

instance forall s . MonadFail (StateIO s) where
  fail = error

{-
(>>=) :: forall s a b . StateIO s a -> (a -> StateIO s b) -> StateIO s b
(>>=) m k = S $ \ s -> IO.do
  (a, ss) <- runStateIO m s
  runStateIO (k a) ss

(>>) :: forall s a b . StateIO s a -> StateIO s b -> StateIO s b
(>>) m k = S $ \ s -> IO.do
  (_, ss) <- runStateIO m s
  runStateIO k ss

return :: forall s a . a -> StateIO s a
return a = S $ \ s -> IO.return (a, s)

fmap :: forall s a b . (a -> b) -> StateIO s a -> StateIO s b
fmap f sa = S $ \ s -> IO.do
  (a, ss) <- runStateIO sa s
  IO.return (f a, ss)

fail :: forall s a . String -> StateIO s a
fail = error

when :: forall s . Bool -> StateIO s () -> StateIO s ()
when b s = if b then s else return ()

-}

gets :: forall s a . (s -> a) -> StateIO s a
gets f = S $ \ s -> return (f s, s)

modify :: forall s . (s -> s) -> StateIO s ()
modify f = S $ \ s -> return ((), f s)

put :: forall s . s -> StateIO s ()
put s = S $ \ _ -> return ((), s)

get :: forall s . StateIO s s
get = S $ \ s -> return (s, s)

liftIO :: forall s a . IO a -> StateIO s a
liftIO io = S $ \ s -> do
  a <- io
  return (a, s)
