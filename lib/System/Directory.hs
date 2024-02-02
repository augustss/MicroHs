module System.Directory(removeFile) where
import Prelude
import Control.Monad(when)
import Foreign.C.String
import Foreign.Ptr

foreign import ccall "unlink" c_unlink :: CString -> IO Int

removeFile :: FilePath -> IO ()
removeFile fn = do
  r <- withCAString fn c_unlink
  when (r /= 0) $
    error "removeFile failed"
