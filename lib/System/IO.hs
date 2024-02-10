-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO(
  IO, Handle, FilePath,
  IOMode(..),
  stdin, stdout, stderr,
  hGetChar, hPutChar,
  putChar, getChar,
  hClose, hFlush,
  openFile, openFileM, openBinaryFile,
  hPutStr, hPutStrLn,
  putStr, putStrLn,
  print,
  hGetContents, getContents,
  hGetLine, getLine,
  interact,
  writeFile, readFile, appendFile,

  hSerialize, hDeserialize, cprint,
  writeSerialized, readSerialized,

  mkTextEncoding, hSetEncoding, utf8,

  getTimeMilli,
  openTmpFile,

  withFile,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.Fail
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.List
import Data.Maybe
import Data.Num
import Data.Tuple
import Text.Show
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO.Unsafe

data FILE
data BFILE
newtype Handle = Handle (Ptr BFILE)

primHSerialize   :: forall a . Ptr BFILE -> a -> IO ()
primHSerialize    = primitive "IO.serialize"
primHPrint       :: forall a . Ptr BFILE -> a -> IO ()
primHPrint        = primitive "IO.print"
primHDeserialize :: forall a . Ptr BFILE -> IO a
primHDeserialize  = primitive "IO.deserialize"
primStdin        :: Ptr FILE
primStdin         = primitive "IO.stdin"
primStdout       :: Ptr FILE
primStdout        = primitive "IO.stdout"
primStderr       :: Ptr FILE
primStderr        = primitive "IO.stderr"

foreign import ccall "fopen"        c_fopen        :: CString -> CString -> IO (Ptr FILE)
{-
foreign import ccall "fclose"       c_fclose       :: Handle             -> IO Int
foreign import ccall "fflush"       c_fflush       :: Handle             -> IO Int
foreign import ccall "fgetc"        c_fgetc        :: Handle             -> IO Int
foreign import ccall "fputc"        c_fputc        :: Int ->     Handle  -> IO Int
-- foreign import ccall "fwrite"       c_fwrite       :: CString -> Int -> Int -> Handle -> IO Int
-}
foreign import ccall "getTimeMilli" c_getTimeMilli ::                       IO Int

foreign import ccall "closeb"       c_closeb       :: Ptr BFILE          -> IO ()
foreign import ccall "flushb"       c_flushb       :: Ptr BFILE          -> IO ()
foreign import ccall "getb"         c_getb         :: Ptr BFILE          -> IO Int
foreign import ccall "putb"         c_putb         :: Int ->  Ptr BFILE  -> IO ()
foreign import ccall "add_FILE"     c_add_FILE     :: Ptr FILE           -> IO (Ptr BFILE)
foreign import ccall "add_utf8"     c_add_utf8     :: Ptr BFILE          -> IO (Ptr BFILE)

----------------------------------------------------------

instance Eq Handle where
  Handle p == Handle q  =  p == q

instance Show Handle where
  show (Handle p) = "Handle-" ++ show p

type FilePath = String

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

instance Functor IO where
  fmap f ioa   = ioa `primBind` \ a -> primReturn (f a)
instance Applicative IO where
  pure         = primReturn
  (<*>)        = ap
instance Monad IO where
  (>>=)        = primBind
  (>>)         = primThen
  return       = primReturn
instance MonadFail IO where
  fail         = error

hSerialize   :: forall a . Handle -> a -> IO ()
hSerialize (Handle p) = primHSerialize p

hDeserialize :: forall a . Handle -> IO a
hDeserialize (Handle p) = primHDeserialize p

stdin        :: Handle
stdin        = bFILE primStdin
stdout       :: Handle
stdout       = bFILE primStdout
stderr       :: Handle
stderr       = bFILE primStderr

bFILE :: Ptr FILE -> Handle
bFILE = Handle . primPerformIO . (c_add_utf8 <=< c_add_FILE)

hClose       :: Handle -> IO ()
hClose (Handle p) = c_closeb p

hFlush       :: Handle -> IO ()
hFlush (Handle p) = c_flushb p

hGetChar :: Handle -> IO Char
hGetChar (Handle p) = do
  c <- c_getb p
  if c == (-1::Int) then
    error "hGetChar: EOF"
   else
    return (chr c)

hPutChar :: Handle -> Char -> IO ()
hPutChar (Handle p) c = c_putb (ord c) p

openFILEM :: FilePath -> IOMode -> IO (Maybe (Ptr FILE))
openFILEM p m = do
  let
    ms = case m of
          ReadMode -> "r"
          WriteMode -> "w"
          AppendMode -> "a"
          ReadWriteMode -> "w+"
  h <- withCAString p $ \cp -> withCAString ms $ \ cm -> c_fopen cp cm
  if h == nullPtr then
    return Nothing
   else
    return (Just h)

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM p m = do
  mf <- openFILEM p m
  case mf of
    Nothing -> return Nothing
    Just p -> do { q <- c_add_utf8 =<< c_add_FILE p; return (Just (Handle q)) }

openFile :: String -> IOMode -> IO Handle
openFile p m = do
  mh <- openFileM p m
  case mh of
    Nothing -> error ("openFile: cannot open " ++ p)
    Just h -> return h

putChar :: Char -> IO ()
putChar = hPutChar stdout

getChar :: IO Char
getChar = hGetChar stdin

cprint :: forall a . a -> IO ()
cprint a = primRnfNoErr a `seq` primHPrint p a
  where Handle p = stdout

cuprint :: forall a . a -> IO ()
cuprint = primHPrint p
  where Handle p = stdout

print :: forall a . (Show a) => a -> IO ()
print a = putStrLn (show a)

putStr :: String -> IO ()
putStr = hPutStr stdout

hPutStr :: Handle -> String -> IO ()
hPutStr h = mapM_ (hPutChar h)

putStrLn :: String -> IO ()
putStrLn = hPutStrLn stdout

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

hGetLine :: Handle -> IO String
hGetLine h = loop ""
  where loop s = do
          c <- hGetChar h
          if c == '\n' then
            return (reverse s)
           else
            loop (c:s)

getLine :: IO String
getLine = hGetLine stdin

writeFile :: FilePath -> String -> IO ()
writeFile p s = do
  h <- openFile p WriteMode
  hPutStr h s
  hClose h

appendFile :: FilePath -> String -> IO ()
appendFile p s = do
  h <- openFile p AppendMode
  hPutStr h s
  hClose h

{-
-- Faster, but uses a lot more C memory.
writeFileFast :: FilePath -> String -> IO ()
writeFileFast p s = do
  h <- openFile p WriteMode
  (cs, l) <- newCAStringLen s
  n <- c_fwrite cs 1 l h
  free cs
  hClose h
  when (l /= n) $
    error "writeFileFast failed"
-}

-- Lazy readFile
readFile :: FilePath -> IO String
readFile p = do
  h <- openFile p ReadMode
  cs <- hGetContents h
  --hClose h  can't close with lazy hGetContents
  return cs

-- Lazy hGetContents
hGetContents :: Handle -> IO String
hGetContents h@(Handle p) = do
  c <- c_getb p
  if c == (-1::Int) then do
    hClose h   -- EOF, so close the handle
    return ""
   else do
    cs <- unsafeInterleaveIO (hGetContents h)
    return (chr c : cs)
  
getContents :: IO String
getContents = hGetContents stdin

interact :: (String -> String) -> IO ()
interact f = getContents >>= putStr . f

writeSerialized :: forall a . FilePath -> a -> IO ()
writeSerialized p s = do
  h <- openFile p WriteMode
  hSerialize h s
  hClose h

readSerialized :: forall a . FilePath -> IO a
readSerialized p = do
  h <- openFile p ReadMode
  a <- hDeserialize h
  hClose h
  return a

getTimeMilli :: IO Int
getTimeMilli = c_getTimeMilli

openBinaryFile :: String -> IOMode -> IO Handle
openBinaryFile p m = do
  mf <- openFILEM p m
  case mf of
    Nothing -> error $ "openBinaryFile: cannot open " ++ show p
    Just p -> do { q <- c_add_FILE p; return (Handle q) }

--------
-- For compatibility

data TextEncoding = UTF8

utf8 :: TextEncoding
utf8 = UTF8

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding "UTF-8//ROUNDTRIP" = return UTF8
mkTextEncoding _ = error "unknown text encoding"

-- Always in UTF8 mode
hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding _ _ = return ()

--------

-- Create a temporary file, take a prefix and a suffix
-- and returns a malloc()ed string.
foreign import ccall "tmpname" c_tmpname :: CString -> CString -> IO CString

-- Create and open a temporary file.
openTmpFile :: String -> IO (String, Handle)
openTmpFile tmpl = do
  let (pre, suf) =
        case span (/= '.') (reverse tmpl) of
          (rsuf, "") -> (tmpl, "")
          (rsuf, _:rpre) -> (reverse rpre, '.':reverse rsuf)
  ctmp <- withCAString pre $ withCAString suf . c_tmpname
  tmp <- peekCAString ctmp
  free ctmp
  h <- openFile tmp WriteMode
  return (tmp, h)

-- XXX needs bracket
withFile :: forall r . FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile fn md io = do
  h <- openFile fn md
  r <- io h
  hClose h
  return r
