-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE QualifiedDo #-}
-- State monad over IO
module MicroHs.StateIO(module MicroHs.StateIO) where
import Prelude --Xhiding (Monad(..), mapM)
import qualified System.IO as IO
--Ximport qualified CompatIO as IO

data StateIO s a = S (s -> IO (a,s))

runStateIO :: forall s a . StateIO s a -> (s -> IO (a,s))
runStateIO sa =
  case sa of
    S x -> x

execStateIO :: forall s a . StateIO s a -> s -> IO s
execStateIO sa s = IO.do
  as <- runStateIO sa s
  case as of
    (_, ss) -> IO.return ss

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

gets :: forall s a . (s -> a) -> StateIO s a
gets f = S $ \ s -> IO.return (f s, s)

when :: forall s . Bool -> StateIO s () -> StateIO s ()
when b s = if b then s else MicroHs.StateIO.return ()

modify :: forall s . (s -> s) -> StateIO s ()
modify f = S $ \ s -> IO.return ((), f s)

put :: forall s . s -> StateIO s ()
put s = S $ \ _ -> IO.return ((), s)

get :: forall s . StateIO s s
get = S $ \ s -> IO.return (s, s)

liftIO :: forall s a . IO a -> StateIO s a
liftIO io = S $ \ s -> IO.do
  a <- io
  IO.return (a, s)

mapM :: forall s a b . (a -> StateIO s b) -> [a] -> StateIO s [b]
mapM f =
  let
    rec arg =
      case arg of
        [] -> MicroHs.StateIO.return []
        a : as -> MicroHs.StateIO.do
          b <- f a
          bs <- rec as
          MicroHs.StateIO.return (b : bs)
  in rec
