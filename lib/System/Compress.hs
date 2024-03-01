module System.Compress(compress) where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

foreign import ccall "lz77c" c_lz77c :: CString -> CSize -> Ptr CString -> IO CSize

-- This really ought to be [Word8] -> [Word8]
compress :: String -> String
compress file = unsafePerformIO $ do
  (iptr, ilen) <- newCAStringLen file
  pptr <- new nullPtr
  olen <- c_lz77c iptr (intToCSize ilen) pptr
  optr <- peek pptr
  res <- peekCAStringLen (optr, cSizeToInt olen)
  free iptr
  free optr
  return res
