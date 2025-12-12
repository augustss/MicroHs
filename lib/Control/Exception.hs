-- Copyright 2023, 2024 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Exception(
  -- re-exports
  throw,
  SomeException(..),
  Exception(..),
  --
  Handler(..), catches,
  --
  catch, catchJust,
  handle, handleJust,

  bracket, finally, bracket_, bracketOnError,

  try,
  tryJust,
  throwIO,
  onException,
  displaySomeException,
  evaluate,
  mapException,

  mask, mask_,

  allowInterrupt,

  uninterruptibleMask,
  uninterruptibleMask_,
  interruptible,
  MaskingState(..),
  getMaskingState,
  unsafeUnmask,

  --
  ArithException(..),
  SomeAsyncException(..), AsyncException(..),
  asyncExceptionToException,
  asyncExceptionFromException,
  --
  BlockedIndefinitelyOnMVar(..), blockedIndefinitelyOnMVar,
  BlockedIndefinitelyOnSTM(..), blockedIndefinitelyOnSTM,
  --
  ErrorCall(..),
  --
  ioError, IOException,
  --
  assert, assertError,
  ) where
import qualified Prelude(); import MiniPrelude
import Control.Exception.Internal
import {-# SOURCE #-} Data.Typeable
import System.IO.Error(ioError, IOException)
import System.IO.Unsafe

-- This is the function called by the runtime.
-- It compiles to
--    (U (U (K2 A)))
displaySomeException :: SomeException -> String
displaySomeException = displayException

--------------------------

data Handler a = forall e . Exception e => Handler (e -> IO a)

instance Functor Handler where
     fmap f (Handler h) = Handler (fmap f . h)

catches :: IO a -> [Handler a] -> IO a
catches io handlers = io `catch` catchesHandler handlers

catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res
              = maybe res handler (fromException e)

--------------------------

catchJust
        :: Exception e
        => (e -> Maybe b)
        -> IO a
        -> (b -> IO a)
        -> IO a
catchJust p a handler = catch a handler'
  where handler' e = case p e of
                        Nothing -> throwIO e
                        Just b  -> handler b

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle = flip catch

handleJust :: Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p = flip (catchJust p)

mapException :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a
mapException f v =
  unsafePerformIO (catch (evaluate v)
                         (throwIO . f))

-- Evaluate a when executed, not when evaluated
evaluate :: a -> IO a
evaluate a = seq a (return ()) >> return a

try :: forall a e . Exception e => IO a -> IO (Either e a)
try ioa = catch (fmap Right ioa) (return . Left)

tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)
tryJust p a = catchJust p (Right `fmap` a) (return . Left)

{-
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
-- defined in Control.Exception.Internal
-}

mask_ :: IO a -> IO a
mask_ io = mask $ const io

finally :: IO a -> IO b -> IO a
finally a sequel =
  mask $ \ restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r

bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError before after thing =
  mask $ \ restore -> do
    a <- before
    restore (thing a) `onException` after a

allowInterrupt :: IO ()
allowInterrupt = interruptible $ return ()

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ :: forall a b c. IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

-------------------

deriving instance Eq   ArithException
deriving instance Ord  ArithException
deriving instance Eq   AsyncException
deriving instance Ord  AsyncException
deriving instance Eq   MaskingState
deriving instance Show MaskingState

blockedIndefinitelyOnMVar :: SomeException
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

blockedIndefinitelyOnSTM :: SomeException -- for the RTS
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

-------------------

-- Assertions are ignored.
-- GHC has a flag to turn assert into assertError, MHS doesn't.
assert :: Bool -> a -> a
assert _pred r = r

assertError :: Bool -> a -> a
assertError predicate v
  | predicate = v
  | otherwise = throw (AssertionFailed "Assertion failed")

newtype AssertionFailed = AssertionFailed String

instance Exception AssertionFailed

instance Show AssertionFailed where
  showsPrec _ (AssertionFailed err) = showString err
    
