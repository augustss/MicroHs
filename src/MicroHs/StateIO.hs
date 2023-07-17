-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# LANGUAGE QualifiedDo #-}
-- State monad over IO
module MicroHs.StateIO(module MicroHs.StateIO) where
import Prelude --Xhiding (Monad(..), mapM)
import System.IO as IO
--Ximport qualified CompatIO as IO

data StateIO s a = S (s -> IO (a,s))

runStateIO :: StateIO s a -> (s -> IO (a,s))
runStateIO sa =
  case sa of
    S x -> x

execStateIO :: StateIO s a -> s -> IO s
execStateIO sa s = IO.do
  as <- runStateIO sa s
  case as of
    (_, ss) -> IO.return ss

(>>=) :: StateIO s a -> (a -> StateIO s b) -> StateIO s b
(>>=) m k = S $ \ s -> IO.do
  (a, ss) <- runStateIO m s
  runStateIO (k a) ss

(>>) :: StateIO s a -> StateIO s b -> StateIO s b
(>>) m k = S $ \ s -> IO.do
  (_, ss) <- runStateIO m s
  runStateIO k ss

return :: a -> StateIO s a
return a = S $ \ s -> IO.return (a, s)

fmap :: (a -> b) -> StateIO s a -> StateIO s b
fmap f sa = S $ \ s -> IO.do
  (a, ss) <- runStateIO sa s
  IO.return (f a, ss)

gets :: (s -> a) -> StateIO s a
gets f = S $ \ s -> IO.return (f s, s)

when :: Bool -> StateIO s () -> StateIO s ()
when b s = if b then s else MicroHs.StateIO.return ()

modify :: (s -> s) -> StateIO s ()
modify f = S $ \ s -> IO.return ((), f s)

put :: s -> StateIO s ()
put s = S $ \ _ -> IO.return ((), s)

get :: StateIO s s
get = S $ \ s -> IO.return (s, s)

liftIO :: IO a -> StateIO s a
liftIO io = S $ \ s -> IO.do
  a <- io
  IO.return (a, s)

mapM :: (a -> StateIO s b) -> [a] -> StateIO s [b]
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

