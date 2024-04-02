-- Copyright 2023, 2024 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Exception(
  -- re-exports
  catch, throw,
  SomeException,
  Exception(..),
  -- defined here
  try,
  throwIO,
--  onException,
  ) where
import Control.Exception.Internal
import {-# SOURCE #-} Data.Typeable

instance Show SomeException where
  showsPrec p (SomeException e) = showsPrec p e

instance Exception SomeException where
  toException se = se
  fromException = Just
  displayException (SomeException e) = displayException e

-- This is the function called by the runtime.
-- It compiles to
--    (U (U (K2 A)))
-- displaySomeException :: SomeException -> String
-- displaySomeException = displayException

--------------------------

try :: forall a e . Exception e => IO a -> IO (Either e a)
try ioa = catch (fmap Right ioa) (return . Left)

throwIO :: forall a e . Exception e => e -> IO a
throwIO e = throw e

{-
onException :: IO a -> IO b -> IO a
onException io what =
  io `catch` \ e -> do { _ <- what; throwIO e }
-}
