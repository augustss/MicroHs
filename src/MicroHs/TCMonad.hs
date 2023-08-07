module MicroHs.TCMonad(
  module MicroHs.TCMonad,
  module Control.Monad.State.Strict,
{-
  TC,
  (>>=), (>>), return,
  runState,
  fmap,
  fail,
  mapM, mapM_,
  get, gets, put,
-}
--X(<$>),
  ) where
--import Control.Monad
import Control.Monad.State.Strict --Xhiding(ap)

type TC s a = State s a
