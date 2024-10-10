module Control.Monad.IO.Class (MonadIO(..)) where
import Prelude()
import Data.Char
import Control.Applicative
import Control.Monad
import System.IO

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO io = io
