-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO(
  module System.IO, Handle, IO,
  module Data.Functor,
  module Control.Applicative,
  module Control.Monad,
  ) where
import Primitives
import Control.Applicative
import Control.Error
import Control.Monad
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

data FILE
newtype Handle = Handle (Ptr FILE)

primHSerialize   :: forall a . Handle -> a -> IO ()
primHSerialize    = primitive "IO.serialize"
primHPrint       :: forall a . Handle -> a -> IO ()
primHPrint        = primitive "IO.print"
primHDeserialize :: forall a . Handle -> IO a
primHDeserialize  = primitive "IO.deserialize"
primStdin        :: Handle
primStdin         = primitive "IO.stdin"
primStdout       :: Handle
primStdout        = primitive "IO.stdout"
primStderr       :: Handle
primStderr        = primitive "IO.stderr"

foreign import ccall "fopen"        c_fopen        :: CString -> CString -> IO Handle
foreign import ccall "fclose"       c_fclose       :: Handle             -> IO Int
foreign import ccall "fflush"       c_fflush       :: Handle             -> IO Int
foreign import ccall "fgetc"        c_fgetc        :: Handle             -> IO Int
foreign import ccall "fputc"        c_fputc        :: Int ->     Handle  -> IO Int
foreign import ccall "fwrite"       c_fwrite       :: CString -> Int -> Int -> Handle -> IO Int
foreign import ccall "getTimeMilli" c_getTimeMilli ::                       IO Int

----------------------------------------------------------

instance Eq Handle where
  Handle p == Handle q  =  p == q

instance Show Handle where
  show (Handle p) = "Handle-" ++ show p

nullHandle :: Handle
nullHandle = Handle nullPtr

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
hSerialize   = primHSerialize

hDeserialize :: forall a . Handle -> IO a
hDeserialize = primHDeserialize

stdin        :: Handle
stdin        = primStdin
stdout       :: Handle
stdout       = primStdout
stderr       :: Handle
stderr       = primStderr

hClose       :: Handle -> IO ()
hClose h     = do { c_fclose h; return () }  -- ignore error code

hFlush       :: Handle -> IO ()
hFlush h     = do { c_fflush h; return () }  -- ignore error code

hGetChar :: Handle -> IO Char
hGetChar h = do
  c <- c_fgetc h
  if c == (-1::Int) then
    error "hGetChar: EOF"
   else
    return (chr c)

hPutChar :: Handle -> Char -> IO ()
hPutChar h c = do { c_fputc (ord c) h; return () }  -- ignore error code

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM p m = do
  let
    ms = case m of
          ReadMode -> "r"
          WriteMode -> "w"
          AppendMode -> "a"
          ReadWriteMode -> "w+"
  h <- withCAString p $ \cp -> withCAString ms $ \ cm -> c_fopen cp cm
  if h == nullHandle then
    return Nothing
   else
    return (Just h)

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
cprint a = primRnfNoErr a `seq` primHPrint stdout a

cuprint :: forall a . a -> IO ()
cuprint = primHPrint stdout

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

writeFile :: FilePath -> String -> IO ()
writeFile p s = do
  h <- openFile p WriteMode
  hPutStr h s
  hClose h

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

-- Lazy readFile
readFile :: FilePath -> IO String
readFile p = do
  h <- openFile p ReadMode
  cs <- hGetContents h
  --hClose h  can't close with lazy hGetContents
  return cs

-- Lazy hGetContents
hGetContents :: Handle -> IO String
hGetContents h = do
  c <- c_fgetc h
  if c == (-1::Int) then do
    hClose h   -- EOF, so close the handle
    return ""
   else do
    cs <- unsafeInterleaveIO (hGetContents h)
    return (chr c : cs)
  
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

unsafeInterleaveIO :: forall a . IO a -> IO a
unsafeInterleaveIO ioa = return (primPerformIO ioa)

seq :: forall a b . a -> b -> b
seq = primSeq

-- MicroHs is always in binary mode
hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode _ _ = return ()

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

--------

-- Helper for interactive system
class PrintOrRun a where
  printOrRun :: a -> IO ()

instance PrintOrRun (IO ()) where
  printOrRun a = a

instance forall a . Show a => PrintOrRun a where
  printOrRun a = putStrLn (show a)
