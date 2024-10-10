-- Copyright 2023, 2024 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Exception(
  -- re-exports
  throw,
  SomeException,
  Exception(..),
  --
  Handler(..), catches,
  --
  catch, catchJust,
  handle, handleJust,

  bracket, finally, bracket_, bracketOnError,
  
  try,
  throwIO,
  onException,
  displaySomeException,
  --
  ArithException(..),
  ) where
import Prelude(); import MiniPrelude
import Control.Exception.Internal
import {-# SOURCE #-} Data.Typeable
import System.IO.Unsafe

instance Show SomeException where
  showsPrec p (SomeException e) = showsPrec p e

instance Exception SomeException where
  toException se = se
  fromException = Just
  displayException (SomeException e) = displayException e

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
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

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
                         (\x -> throwIO (f x)))

-- Evaluate a when executed, not when evaluated
evaluate :: a -> IO a
evaluate a = seq a (return ()) >> return a

try :: forall a e . Exception e => IO a -> IO (Either e a)
try ioa = catch (fmap Right ioa) (return . Left)

-- Throw an exception when executed, not when evaluated
throwIO :: forall a e . Exception e => e -> IO a
throwIO e = bad >> bad   -- we never reacj the second 'bad'
  where bad = throw e

onException :: IO a -> IO b -> IO a
onException io what =
  io `catch` \ e -> do _ <- what
                       throwIO (e :: SomeException)

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
  mask $ \ restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

finally :: IO a -> IO b -> IO a
finally a sequel =
  mask $ \ restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError before after thing =
  mask $ \ restore -> do
    a <- before
    restore (thing a) `onException` after a

-- XXX we don't have masks yet
mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
mask io = io id

-------------------

data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator
  deriving (Eq, Ord, Show, Typeable)
instance Exception ArithException
