module Control.Concurrent.STM.Internal(
  STM,
  TVar,
  newTVar,
  writeTVar,
  readTVar,
  orElse,
  retry,
  ) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Typeable

newtype Set a = Set [a]

newtype Map k v = Map [(k, v)]

-----------------------------------------------------------

newtype TVar a = TVar (TVarA a, TVarAny)

newtype TVarA a = TVarA (MVar (ITVar a))

type TVarId = Int

data TVarAny = forall a. TVarAny (TVarId, MVar (ITVar a))

data ITVar a = TV
  { globalContent :: MVar a
  , localContent :: MVar (Map ThreadId (IORef [a]))
  , notifyList :: MVar (Set ThreadId)
  , lock :: MVar ThreadId
  , waitingQueue :: MVar [MVar ()]
  }

data Log = Log
  { readTVars :: Set TVarAny
  , tripelStack :: [(Set TVarAny,Set TVarAny,Set TVarAny)]
  , lockingSet :: Set TVarAny
  }
newtype TLOG = TLOG (IORef Log)

data STM a
  = Return a
  | Retry
  | forall b. NewTVar b (TVar b -> STM a)
  | forall b. ReadTVar (TVar b) (b -> STM a)
  | forall b. WriteTVar (TVar b) b (STM a)
  | forall b. OrElse (STM b) (STM b) (b -> STM a)

newTVar :: a -> STM (TVar a)
newTVar a = NewTVar a return

writeTVar :: TVar a -> a -> STM ()
writeTVar v x = WriteTVar v x (return ())

readTVar :: TVar a -> STM a
readTVar a = ReadTVar a return

orElse :: STM a -> STM a -> STM a
orElse a1 a2 = OrElse a1 a2 return
  
retry :: STM a
retry = Retry

instance Functor STM where
  fmap f x = x >>= return . f

instance Applicative STM where
  pure = Return
  (<*>) = ap

instance Monad STM where
  m >>= f =
    case m of
      Return x         -> f x
      Retry            -> Retry
      NewTVar x ct     -> NewTVar x (\ i -> ct i >>= f)
      ReadTVar x ct    -> ReadTVar x (\ i -> ct i >>= f)
      WriteTVar v x ct -> WriteTVar v x (ct >>= f)
      OrElse a1 a2 ct  -> OrElse a1 a2 (\ i -> ct i >>= f)

atomically :: STM a -> IO a
atomically act = do
  tlog <- emptyTLOG
  catch (performSTM tlog act)
        (\ e -> case e of
            RetryException ->
              (uninterruptibleMask_ (globalRetry tlog)) >> atomically act
            _ -> throw e)

data RetryException = RetryException
  deriving(Show, Typeable)
instance Exception RetryException

emptyTLOG :: IO TLOG
emptyTLOG = undefined

performSTM :: TLOG -> STM a -> IO a
performSTM = undefined

globalRetry :: TLOG -> IO ()
globalRetry = undefined
