-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module GHC.Stack(
  HasCallStack(..),
  withFrozenCallStack,
  ) where
import qualified Prelude()              -- do not import Prelude
import {-# SOURCE #-} Data.Typeable
-- So we can import GHC.Stack.
-- Sadly, this does not give us stack traces. :(

class HasCallStack
instance HasCallStack

withFrozenCallStack :: HasCallStack => (HasCallStack => a) -> a
withFrozenCallStack a = a
