module Foreign.StablePtr(
  StablePtr,
  newStablePtr,
  deRefStablePtr,
  freeStablePtr,
  castStablePtrToPtr,
  castPtrToStablePtr,
  ) where
import qualified Prelude()
import Mhs.Builtin
import MiniPrelude
import Primitives
import Data.Typeable
import Foreign.Storable

newtype StablePtr a = StablePtr Word
  deriving (Typeable)
  deriving newtype (Storable)

instance Eq (StablePtr a) where
  StablePtr a == StablePtr a'  =  a == a'

instance Show (StablePtr a) where
  show (StablePtr sp) = "StablePtr#" ++ show sp

newStablePtr :: a -> IO (StablePtr a)
newStablePtr a = StablePtr <$> primNewStablePtr a

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr (StablePtr sp) = primDeRefStablePtr sp

freeStablePtr :: StablePtr a -> IO ()
freeStablePtr (StablePtr sp) = primFreeStablePtr sp

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr sp) = primWordToPtr sp

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr = StablePtr . primPtrToWord
