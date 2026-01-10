module Example where
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Functor.Identity

newtype TardisT bw fw m a = TardisT
  { runTardisT :: (bw, fw) -> m (a, (bw, fw))
  }

tardis :: Monad m => ((bw, fw) -> (a, (bw, fw))) -> TardisT bw fw m a
tardis f = TardisT $ \s -> return (f s)

instance MonadFix m => Monad (TardisT bw fw m) where

instance MonadFix m => Functor (TardisT bw fw m) where

instance MonadFix m => Applicative (TardisT bw fw m) where

instance MonadFix m => MonadFix (TardisT bw fw m) where
  mfix f = TardisT $ \s -> mdo
    (x, s') <- runTardisT (f x) s
    return (x, s')


class (Applicative m, MonadFix m) => MonadTardis bw fw m where -- | m -> fw where
  getFuture  :: m bw
  getFuture = undefined

instance MonadFix m => MonadTardis bw fw (TardisT bw fw m) where
  getFuture = tardis $ \ ~(bw, fw)  -> (bw, (bw, fw))

gogo :: TardisT Int Int Identity Int
gogo = getFuture

