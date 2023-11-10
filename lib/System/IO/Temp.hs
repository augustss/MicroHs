module System.IO.Temp(
  withSystemTempFile
  ) where
import Prelude
import System.Directory
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc

foreign import ccall "tempnam" c_tempnam :: CString -> CString -> IO CString

withSystemTempFile :: forall a . String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile tmpl io = do
  let (pre, post) = span (/= '.') tmpl
  ctmp <- withCAString pre $ c_tempnam nullPtr
  tmp <- peekCAString ctmp
  free ctmp
  let fn = tmp ++ post
  h <- openFile fn WriteMode
  a <- io fn h
  hClose h
  removeFile fn
  return a
