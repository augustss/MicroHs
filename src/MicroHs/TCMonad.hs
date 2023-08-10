{-# OPTIONS_GHC -Wno-orphans #-}
module MicroHs.TCMonad(
--X(<$>),
  module MicroHs.TCMonad,
  module Control.Monad.State.Strict
{-
  TC,
  (>>=), (>>), return,
  runState,
  fmap,
  fail,
  mapM, mapM_,
  get, gets, put,
-}
  ) where
--import Control.Monad
--Ximport Data.Functor.Identity
import Control.Monad.State.Strict --Xhiding(ap)

type TC s a = State s a

--Xinstance MonadFail Identity where fail = error
