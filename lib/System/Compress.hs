module System.Compress(compress) where
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

foreign import ccall "lz77c" c_lz77c :: CString -> Int -> Ptr CString -> IO Int

-- This really ought to be [Word8] -> [Word8]
compress :: String -> String
compress file = unsafePerformIO $ do
  (iptr, ilen) <- newCAStringLen file
  pptr <- new nullPtr
  olen <- c_lz77c iptr ilen pptr
  optr <- peek pptr
  res <- peekCAStringLen (optr, olen)
  free iptr
  free optr
  return res
