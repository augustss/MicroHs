-- Low level interface to byte arrays.
module Data.Array.Byte(
  ByteArray,
  sizeOfByteArray,
  newByteArray,
  indexWord8,
  byteArrayToByteString,
  byteStringToByteArray,
  withByteArrayPtr,

  MutableByteArray,
  sizeOfMutableByteArray,
  newMutableByteArray,
  freezeMutableByteArray,
  unsafeFreezeMutableByteArray,
  readWord8,
  writeWord8,
  withMutableByteArrayPtr,
  ) where
import Control.Monad.ST
import Control.Monad.ST_Type
import Data.ByteString as BS
import Data.ByteString.Internal(primBS2FPtr)
import Data.Word
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Ptr(Ptr, castPtr)

newtype ByteArray = A BS.ByteString
  deriving newtype (Eq, Ord, Show)

sizeOfByteArray :: ByteArray -> Int
sizeOfByteArray (A bs) = BS.length bs

newByteArray :: [Word8] -> ByteArray
newByteArray bs = A (BS.pack bs)

indexWord8 :: ByteArray -> Int -> Word8
indexWord8 (A bs) i = BS.index bs i

byteStringToByteArray :: BS.ByteString -> ByteArray
byteStringToByteArray bs = A bs

byteArrayToByteString :: ByteArray -> BS.ByteString
byteArrayToByteString (A bs) = bs

withByteArrayPtr :: ByteArray -> (Ptr Word8 -> IO a) -> IO a
withByteArrayPtr (A bs) act = withForeignPtr (primBS2FPtr bs) (act . castPtr)

----------------------------------------------------

-- Be careful about sequencing since the ByteString
-- operations have pure types.

newtype MutableByteArray s = M BS.ByteString

newMutableByteArray :: Int -> ST s (MutableByteArray s)
newMutableByteArray n = return $! M (BS.replicate n 0)

freezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
freezeMutableByteArray (M bs) = return $! A (BS.copy bs)

writeWord8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeWord8 (M bs) n b = ST (primBSwrite bs n b)

readWord8 :: MutableByteArray s -> Int -> ST s Word8
readWord8 (M bs) i = return $! BS.index bs i

sizeOfMutableByteArray :: MutableByteArray s -> ST s Int
sizeOfMutableByteArray (M bs) = return $! BS.length bs

primBSwrite :: BS.ByteString -> Int -> Word8 -> IO ()
primBSwrite = _primitive "bswrite"

unsafeFreezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeMutableByteArray (M bs) = return (A bs)

withMutableByteArrayPtr :: MutableByteArray s -> (Ptr Word8 -> IO a) -> IO a
withMutableByteArrayPtr (M bs) act = withForeignPtr (primBS2FPtr bs) (act . castPtr)
