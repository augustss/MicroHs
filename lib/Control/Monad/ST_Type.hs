-- This module should not be imported!
module Control.Monad.ST_Type(
  ST(..), unST,
  ) where
import Prelude()              -- do not import Prelude
import Primitives(IO)

-- The ST monad is implemented with the IO monad.
newtype ST s a = ST (IO a)
unST :: forall s a . ST s a -> IO a
unST (ST io) = io
