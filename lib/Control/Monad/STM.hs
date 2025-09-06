module Control.Monad.STM(
  STM,
  atomically,
  retry,
  orElse,
  check,
  throwSTM,
  catchSTM,
  ) where
import Control.Monad
import Control.Concurrent.STM.STMMonad

check :: Bool -> STM ()
check b = unless b retry
