{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports -Wno-unused-imports #-}
module MicroHs.TCMonad(
  TC, tcRun,
  fmap, (<$>), (<*>),
  (>>=), (>>), return, fail,
  get, put, gets,
  mapM, mapM_,
  sequence,
  when,
  tcError
  ) where
--Ximport Control.Monad hiding(ap)
--Ximport Data.Functor.Identity
--Ximport GHC.Stack
import Data.Char  -- for String
import Control.Monad.State.Strict --Xhiding(ap)
import MicroHs.Ident
import MicroHs.Expr

type TC s a = State s a

tcRun :: forall s a . TC s a -> s -> (a, s)
tcRun = runState

tcError :: --XHasCallStack =>
           forall s a . SLoc -> String -> TC s a
tcError = errorMessage

--Xinstance MonadFail Identity where fail = error
