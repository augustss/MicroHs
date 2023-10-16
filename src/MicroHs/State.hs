{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports -Wno-unused-imports #-}
module MicroHs.State(
  State, runState,
  fmap, (<$>), (<*>),
  (>>=), (>>), return, fail,
  get, put, gets,
  mapM, mapM_,
  sequence,
  when,
  ) where
--Ximport Control.Monad hiding(ap)
--Ximport Data.Functor.Identity
--Ximport GHC.Stack
import Control.Monad.State.Strict --Xhiding(ap)
