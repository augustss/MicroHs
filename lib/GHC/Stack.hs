-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module GHC.Stack(HasCallStack(..)) where
import qualified Prelude()              -- do not import Prelude
-- So we can import GHC.Stack.
-- Sadly, this does not give us stack traces. :(

class HasCallStack
instance HasCallStack
