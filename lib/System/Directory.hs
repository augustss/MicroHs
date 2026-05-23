module System.Directory(
  removeFile,
  doesFileExist,
  doesDirectoryExist,
  getDirectoryContents,
  listDirectory,
  setCurrentDirectory,
  getCurrentDirectory,
  withCurrentDirectory,
  createDirectory,
  createDirectoryIfMissing,
  copyFile,
  getHomeDirectory,
  getTemporaryDirectory,
  --
  Permissions(..),
  emptyPermissions,
  setOwnerReadable,
  setOwnerWritable,
  setOwnerExecutable,
  setOwnerSearchable,
  getPermissions,
  setPermissions,
  copyPermissions,
  ) where
import qualified Prelude(); import MiniPrelude
import Control.Exception(bracket)
import Control.Monad(when)
import Data.Bits
import Foreign.C.Error
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO
import System.Environment

data DIR
data Dirent

foreign import ccall "remove"   c_remove   :: CString -> IO Int
foreign import ccall "opendir"  c_opendir  :: CString -> IO (Ptr DIR)
foreign import ccall "closedir" c_closedir :: Ptr DIR -> IO Int
foreign import ccall "readdir"  c_readdir  :: Ptr DIR -> IO (Ptr Dirent)
foreign import ccall "c_d_name" c_d_name   :: Ptr Dirent -> IO CString
foreign import ccall "chdir"    c_chdir    :: CString -> IO Int
foreign import ccall "mkdir"    c_mkdir    :: CString -> Int -> IO Int
foreign import ccall "getcwd"   c_getcwd   :: CString -> Int -> IO CString
foreign import ccall "get_permissions"   c_getPermissions   :: CString -> IO Int
foreign import ccall "set_permissions"   c_setPermissions   :: CString -> Int -> IO Int


-- XXX Use Foreign.C.Error

-- OK on Windows
removeFile :: FilePath -> IO ()
removeFile fn = throwErrnoIfMinus1_ "removeFile" $ withCAString fn c_remove

-- OK on Windows
doesFileExist :: FilePath -> IO Bool
doesFileExist fn = do
  mh <- openFileM fn ReadMode
  case mh of
    Nothing -> return False
    Just h  -> do { hClose h; return True }

-- Uses emulation in windows/extra.c
doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist fn = withCAString fn $ \ cfn -> do
  dp <- c_opendir cfn
  if dp == nullPtr then
    return False
   else do
    c_closedir dp
    return True

-- Uses emulation in windows/extra.c
getDirectoryContents :: FilePath -> IO [String]
getDirectoryContents fn = withCAString fn $ \ cfn -> do
  dp <- throwErrnoIfNull "getDirectoryContents" $ c_opendir cfn
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

-- OK on Windows
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory d = throwErrnoIfMinus1_ "setCurrentDirectory" $ withCAString d c_chdir

-- OK on Windows
getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
  let len = 10000
  allocaBytes len $ \ p -> do
    cwd <- throwErrnoIfNull "getCurrentDirectory" $ c_getcwd p len -- can return NULL if buffer to small
    peekCAString cwd

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir io =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    io

-- OK on Windows???
createDirectory :: FilePath -> IO ()
createDirectory d = throwErrnoIfMinus1_ "createDirectory" $ withCAString d $ \ s -> c_mkdir s 0o775       -- rwxrwxr-x

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing False d = do
  _ <- withCAString d $ \ s -> c_mkdir s 0o775       -- rwxrwxr-x
  return ()
createDirectoryIfMissing True d = do
  let ds = scanl1 (\ x y -> x ++ "/" ++ y) . split [] $ d
      split r [] = [r]
      split r (c:cs) | isPathSeparator c = r : split [] cs
                     | otherwise         = split (r ++ [c]) cs
  mapM_ (createDirectoryIfMissing False) ds

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || _isWindows && c == '\\'

-- XXX does not copy flags
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
  hsrc <- openBinaryFile src ReadMode
  hdst <- openBinaryFile dst WriteMode
  file <- hGetContents hsrc  -- this also closes the file
  hPutStr hdst file
  hClose hdst

getHomeDirectory :: IO FilePath
getHomeDirectory =
  if _isWindows then do
    mhome <- lookupEnv "USERPROFILE"
    case mhome of
      Just home -> return home
      Nothing -> do
        drive <- getEnv "HOMEDRIVE"
        path <- getEnv "HOMEPATH"
        return $ drive ++ "\\" ++ path
  else
    getEnv "HOME"

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory =
  if _isWindows then
    tmp "TMP"    $ tmp "TEMP" $ tmp "USERPROFILE" $ return "C:\\Windows\\Temp"
  else
    tmp "TMPDIR" $ tmp "TNP"  $ tmp "TEMP"        $ return "/tmp"
  where tmp :: String -> IO String -> IO String
        tmp e els = do
          m <- lookupEnv e
          case m of
            Just s -> return s
            Nothing -> els

data Permissions
  = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  , searchable :: Bool
  } deriving (Eq, Ord, Show)

emptyPermissions :: Permissions
emptyPermissions :: Permissions
emptyPermissions = Permissions {
                       readable   = False,
                       writable   = False,
                       executable = False,
                       searchable = False
                   }

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable b p = p { readable = b }

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable b p = p { writable = b }

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable b p = p { executable = b }

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable b p = p { searchable = b }

getPermissions :: FilePath -> IO Permissions
getPermissions fp = withCAString fp $ \ cp -> do
  p <- throwErrnoIfMinus1 "getPermissions" $ c_getPermissions cp
  return Permissions { readable   = (p .&. 4) /= 0
                     , writable   = (p .&. 2) /= 0
                     , executable = (p .&. 1) /= 0
                     , searchable = (p .&. 8) /= 0 }

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions fp p = throwErrnoIfMinus1_ "setPermissions" $ withCAString fp $ \ cp -> c_setPermissions cp ip
  where ip = b readable 4 .|. b writable 2 .|. b executable 1 .|. b searchable 8
        b sel x = if sel p then x else 0

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions src dst = getPermissions src >>= setPermissions dst
