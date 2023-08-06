module MicroHs.TCMonad(
  TC,
  (>>=), (>>), return,
  runState,
  fmap,
  fail,
  mapM, mapM_,
  get, gets, put,
  ) where
--import Control.Monad
import Control.Monad.State.Strict

type TC s a = State s a
