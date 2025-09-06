module Control.Monad.STM(
  STM,
  atomically,
  retry,
  orElse,
  check,
  throwSTM,
  catchSTM,
  ) where
import Control.Concurrent.STM.STMMonad

check :: Bool -> STM ()
check b = if b then return () else retry
