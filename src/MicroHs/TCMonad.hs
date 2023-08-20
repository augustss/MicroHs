{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports -Wno-unused-imports #-}
module MicroHs.TCMonad(
  TC, runState,
  fmap, (<$>),
  (>>=), (>>), return, fail,
  get, put, gets,
  mapM, mapM_
{-
  runState,
  fmap,
  fail,
  mapM, mapM_,
  get, gets, put,
-}
  ) where
--Ximport Control.Monad hiding(ap)
--Ximport Data.Functor.Identity
import Control.Monad.State.Strict --Xhiding(ap)

type TC s a = State s a

--Xinstance MonadFail Identity where fail = error
