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

        -- * Low level interaction with CStrings
        -- ** Using ByteStrings with functions for CStrings
        unsafeUseAsCString,
        unsafeUseAsCStringLen,
{-
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

import qualified Prelude ()
import Primitives
import Data.Word.Word8
import Data.ByteString.Internal
import Foreign.C.String (CString, CStringLen)
import Foreign.ForeignPtr (withForeignPtr)

unsafeHead :: ByteString -> Word8
unsafeHead bs = primBSindex bs 0

unsafeTail :: ByteString -> ByteString
unsafeTail bs = primBSsubstr bs 1 (primBSlength bs `primIntSub` 1)

unsafeInit :: ByteString -> ByteString
unsafeInit bs = primBSsubstr bs 0 (primBSlength bs `primIntSub` 1)

unsafeLast :: ByteString -> Word8
unsafeLast bs = primBSindex bs (primBSlength bs `primIntSub` 1)

unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex = primBSindex

unsafeTake :: Int -> ByteString -> ByteString
unsafeTake n bs = primBSsubstr bs 0 n

unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop n bs = primBSsubstr bs n (primBSlength bs `primIntSub` n)

-- Get actual data pointer out of a ByteString.  There is no copying involved.
-- The RTS retains ownership of the string, and the pointer
-- should not be used after the subcomputations finishes.
unsafeUseAsCString :: ByteString -> (CString -> IO a) -> IO a
unsafeUseAsCString bs act = withForeignPtr (primBS2FPtr bs) act

unsafeUseAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen bs act = withForeignPtr (primBS2FPtr bs) (\p -> act (p, primBSlength bs))
