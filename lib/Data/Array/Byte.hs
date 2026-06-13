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
  newMutableEmptyByteArray,
  newMutableByteArray,
  freezeMutableByteArray,
  unsafeFreezeMutableByteArray,
  readWord8,
  writeWord8,
  withMutableByteArrayPtr,
  appendByte,
  appendChar,
  ) where
import Control.Monad.ST
import Control.Monad.ST_Type
import Data.ByteString as BS
import Data.ByteString.Internal(primBS2FPtr, primBSNE, primBSwrite, primBSread,
                                primBSfreeze, primBSappByte, primBSappChar, primBSnew)
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

newMutableEmptyByteArray :: Int -> ST s (MutableByteArray s)
newMutableEmptyByteArray n = ST (M <$> primBSnew 0 n)

newMutableByteArray :: Int -> ST s (MutableByteArray s)
newMutableByteArray n = ST (M <$> primBSnew n n)

freezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
freezeMutableByteArray (M bs) = return $! A (BS.copy bs)

writeWord8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeWord8 (M bs) n b = ST (primBSwrite bs n b)

readWord8 :: MutableByteArray s -> Int -> ST s Word8
readWord8 (M bs) i = ST (primBSread bs i)

sizeOfMutableByteArray :: MutableByteArray s -> ST s Int
sizeOfMutableByteArray (M bs) = return $! BS.length bs

unsafeFreezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeMutableByteArray (M bs) = ST (A <$> primBSfreeze bs)

withMutableByteArrayPtr :: MutableByteArray s -> (Ptr Word8 -> IO a) -> IO a
withMutableByteArrayPtr (M bs) act = withForeignPtr (primBS2FPtr bs) (act . castPtr)

appendByte :: MutableByteArray s -> Word8 -> ST s ()
appendByte (M bs) b = ST (primBSappByte bs b)

-- Append byte for the UTF8 encoding of the character.
appendChar :: MutableByteArray s -> Char -> ST s ()
appendChar (M bs) b = ST (primBSappChar bs b)
