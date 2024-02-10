module System.Directory(removeFile, doesFileExist) where
import Prelude
import Control.Monad(when)
import Foreign.C.String
import Foreign.Ptr
import System.IO

foreign import ccall "unlink" c_unlink :: CString -> IO Int

removeFile :: FilePath -> IO ()
removeFile fn = do
  r <- withCAString fn c_unlink
  when (r /= 0) $
    error "removeFile failed"

doesFileExist :: FilePath -> IO Bool
doesFileExist fn = do
  mh <- openFileM fn ReadMode
  case mh of
    Nothing -> return False
    Just h  -> do { hClose h; return True }
