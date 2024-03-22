module System.Directory(
  removeFile,
  doesFileExist,
  doesDirectoryExist,
  getDirectoryContents,
  listDirectory,
  setCurrentDirectory,
  ) where
import Prelude
import Control.Monad(when)
import Foreign.C.String
import Foreign.Ptr
import System.IO

data DIR
data Dirent

foreign import ccall "unlink"   c_unlink   :: CString -> IO Int
foreign import ccall "opendir"  c_opendir  :: CString -> IO (Ptr DIR)
foreign import ccall "closedir" c_closedir :: Ptr DIR -> IO Int
foreign import ccall "readdir"  c_readdir  :: Ptr DIR -> IO (Ptr Dirent)
foreign import ccall "c_d_name" c_d_name   :: Ptr Dirent -> IO CString
foreign import ccall "chdir"    c_chdir    :: CString -> IO Int

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

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist fn = withCAString fn $ \ cfn -> do
  dp <- c_opendir cfn
  return False
  if dp == nullPtr then
    return False
   else do
    c_closedir dp
    return True

getDirectoryContents :: FilePath -> IO [String]
getDirectoryContents fn = withCAString fn $ \ cfn -> do
  dp <- c_opendir cfn
  when (dp == nullPtr) $
    error $ "getDirectoryContents: cannot open " ++ fn
  let loop r = do
        de <- c_readdir dp
        if de == nullPtr then do
          c_closedir dp
          return $ reverse r
         else do
          sp <- c_d_name de
          s <- peekCAString sp
          loop (s:r)
  loop []

listDirectory :: FilePath -> IO [String]
listDirectory d = filter (\ n -> n /= "." && n /= "..") <$> getDirectoryContents d

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory d = do
  r <- withCAString d c_chdir
  when (r /= 0) $
    error $ "setCurrentDirectory failed: " ++ d
