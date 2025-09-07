-- These are not really unsafe, but provided for compatibility
module Data.ByteString.Unsafe (

        -- * Unchecked access
        unsafeHead,
        unsafeTail,
        unsafeInit,
        unsafeLast,
        unsafeIndex,
        unsafeTake,
        unsafeDrop,

{-
        -- * Low level interaction with CStrings
        -- ** Using ByteStrings with functions for CStrings
        unsafeUseAsCString,
        unsafeUseAsCStringLen,

        -- ** Converting CStrings to ByteStrings
        unsafePackCString,
        unsafePackCStringLen,
        unsafePackMallocCString,
        unsafePackMallocCStringLen,

        unsafePackAddress,
        unsafePackAddressLen,
        unsafePackCStringFinalizer,
        unsafeFinalize,
-}
  ) where
import Data.Word
import Data.ByteString as BS

unsafeHead :: ByteString -> Word8
unsafeHead = BS.head

unsafeTail :: ByteString -> ByteString
unsafeTail = BS.tail

unsafeInit :: ByteString -> ByteString
unsafeInit = BS.init

unsafeLast :: ByteString -> Word8
unsafeLast = BS.last

unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex = BS.index

unsafeTake :: Int -> ByteString -> ByteString
unsafeTake = BS.take

unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop = BS.drop
