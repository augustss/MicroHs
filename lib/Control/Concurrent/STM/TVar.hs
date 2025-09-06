module Control.Concurrent.STM.TVar (
        TVar,
        newTVar,
        newTVarIO,
        readTVar,
        readTVarIO,
        writeTVar,
        modifyTVar,
        modifyTVar',
        stateTVar,
        swapTVar,
  ) where
import Control.Concurrent.STM.STMMonad

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
    x <- readTVar var
    writeTVar var (f x)

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x

stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar var f = do
   s <- readTVar var
   let !(a, s') = f s
   writeTVar var s'
   return a

swapTVar :: TVar a -> a -> STM a
swapTVar var new = do
    old <- readTVar var
    writeTVar var new
    return old

readTVarIO :: TVar a -> IO a
readTVarIO = atomically . readTVar

{-
mkWeakTVar :: TVar a -> IO () -> IO (Weak (TVar a))
mkWeakTVar t@(TVar t#) (IO finalizer) = IO $ \s ->
    case mkWeak# t# t finalizer s of (# s1, w #) -> (# s1, Weak w #)
-}
