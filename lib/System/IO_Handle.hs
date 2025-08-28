module System.IO_Handle(BFILE, Handle(..), HandleState(..)) where
import qualified Prelude()
import Primitives
import Data.Bool
import Data.Eq
import Data.IORef
import {-# SOURCE #-} Data.Typeable

-- A handle is a ForeignPtr to a C BFILE transducer.
-- It needs to be a ForeignPtr so it can have a finalizer
-- that closes the underlying BFILE when the Handle is gc():ed.

data BFILE  -- tag used for C pointers to BFILE structs
  deriving ()

data Handle = Handle (ForeignPtr BFILE) (IORef HandleState) [Char]

data HandleState = HRead | HWrite | HReadWrite | HSemiClosed | HClosed
  deriving (Eq)
