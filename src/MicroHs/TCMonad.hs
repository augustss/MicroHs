{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports #-}
module MicroHs.TCMonad(
--X  (<$>),
--X  module Control.Monad,
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
--Ximport Control.Monad hiding(ap)
--Ximport Data.Functor.Identity
import Control.Monad.State.Strict --Xhiding(ap)

type TC s a = State s a

--Xinstance MonadFail Identity where fail = error
