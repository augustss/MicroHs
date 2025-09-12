module Mhs.MutUArr(
  MutSTUArr(..),
  newMutSTUArr,
  newMutSTUArrB,
  unsafeReadMutSTUArr,
  unsafeWriteMutSTUArr,
  MutIOUArr(..),
  newMutIOUArr,
  newMutIOUArrB,
  unsafeReadMutIOUArr,
  unsafeWriteMutIOUArr,
  sameByteString,
  withMutIOUArr,
  ) where
import qualified Prelude(); import MiniPrelude
import Control.Monad.ST
import Control.Monad.ST_Type
import Data.ByteString.Internal
import Data.Word.Word8(Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

newtype MutSTUArr s a = MutSTUArr (MutIOUArr a)

newMutSTUArrB :: forall s a . (Storable a) => Word8 -> Int -> ST s (MutSTUArr s a)
newMutSTUArrB b n = ST $ MutSTUArr <$> newMutIOUArrB b n

newMutSTUArr :: forall s a . (Storable a) => Int -> ST s (MutSTUArr s a)
newMutSTUArr n = ST $ MutSTUArr <$> newMutIOUArr n

unsafeReadMutSTUArr :: (Storable a) => MutSTUArr s a -> Int -> ST s a
unsafeReadMutSTUArr (MutSTUArr bs) i = ST $ unsafeReadMutIOUArr bs i

unsafeWriteMutSTUArr :: (Storable a) => MutSTUArr s a -> Int -> a -> ST s ()
unsafeWriteMutSTUArr (MutSTUArr bs) i a = ST $ unsafeWriteMutIOUArr bs i a

newtype MutIOUArr a = MutIOUArr ByteString

newMutIOUArrB :: forall a . (Storable a) => Word8 -> Int -> IO (MutIOUArr a)
newMutIOUArrB b n = return (MutIOUArr (primBSreplicate (n * sizeOf (undefined :: a)) b))

newMutIOUArr :: forall a . (Storable a) => Int -> IO (MutIOUArr a)
newMutIOUArr = newMutIOUArrB 0

unsafeReadMutIOUArr :: (Storable a) => MutIOUArr a -> Int -> IO a
unsafeReadMutIOUArr arr i = withMutIOUArr arr $ \ p -> peekElemOff p i

unsafeWriteMutIOUArr :: (Storable a) => MutIOUArr a -> Int -> a -> IO ()
unsafeWriteMutIOUArr arr i a = withMutIOUArr arr $ \ p -> pokeElemOff p i a

withMutIOUArr :: MutIOUArr a -> (Ptr a -> IO b) -> IO b
withMutIOUArr (MutIOUArr bs) act = withForeignPtr (primBS2FPtr bs) (act . castPtr)
