module Foreign.StablePtr(
  StablePtr,
  newStablePtr,
  deRefStablePtr,
  freeStablePtr,
  castStablePtrToPtr,
  castPtrToStablePtr,
  ) where
import qualified Prelude()
import MiniPrelude
import Primitives
import Data.Typeable

newtype StablePtr a = StablePtr Word
  deriving (Eq, Typeable)

instance Show (StablePtr a) where
  show (StablePtr sp) = "StablePtr#" ++ show sp

newStablePtr :: a -> IO (StablePtr a)
newStablePtr = StablePtr . primNewStablePtr

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr (StablePtr sp) = primDeRefStablePtr sp

freeStablePtr :: StablePtr a -> IO ()
freeStablePtr (StablePtr sp) = primFreeStablePtr sp

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr sp) = primWordToPtr sp

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr = StablePtr . primPtrToWord
