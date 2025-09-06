module Control.Monad.STM(
  STM,
  atomically,
  retry,
  orElse,
{-
  check,
  throwSTM,
  catchSTM,
-}
  ) where
import Control.Concurrent.STM.STMMonad
