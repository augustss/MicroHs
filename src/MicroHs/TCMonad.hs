{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports -Wno-unused-imports #-}
module MicroHs.TCMonad(
  TC, tcRun,
  fmap, (<$>), (<*>),
  (>>=), (>>), return, fail,
  get, put, gets,
  mapM, mapM_,
  sequence,
  tcError
  ) where
import Prelude
import Data.Functor.Identity
import GHC.Stack
import Control.Applicative
import Control.Monad.State.Strict
import Data.Functor
import MicroHs.Ident
import MicroHs.Expr

type TC s a = State s a

tcRun :: forall s a . TC s a -> s -> (a, s)
tcRun = runState

tcError :: --XHasCallStack =>
           forall s a . SLoc -> String -> TC s a
tcError = errorMessage

instance MonadFail Identity where fail = error
