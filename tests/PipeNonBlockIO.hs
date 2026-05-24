module PipeNonBlockIO where

import Control.Monad (void)
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.FD

foreign import ccall "unistd.h pipe"  c_pipe  :: Ptr CInt -> IO CInt
foreign import ccall "unistd.h read"  c_read  :: CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "unistd.h write" c_write :: CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "fcntl.h fcntl"  c_fcntl :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "fcntl.h value F_SETFL"    fSETFL    :: CInt
foreign import capi "fcntl.h value O_NONBLOCK" oNONBLOCK :: CInt

setNonBlocking :: CInt -> IO ()
setNonBlocking fd = throwErrnoIfMinus1_ "fcntl" $ c_fcntl fd fSETFL oNONBLOCK

blockOnRead :: CInt -> IO Word8
blockOnRead fd = alloca go
  where
    go p = do
      r <- c_read fd p 1
      if r == 1
        then peek p
        else do
          errno <- getErrno
          if errno == eAGAIN || errno == eWOULDBLOCK
            then do
              putStrLn "entering waitForReadFD"
              waitForReadFD (fromIntegral fd)
              go p
            else throwErrno "read"

reader :: CInt -> MVar () -> IO ()
reader fd done = do
  putStrLn "reader: waiting for data"
  b1 <- blockOnRead fd
  putStrLn $ "reader: received " ++ show b1
  b2 <- blockOnRead fd
  putStrLn $ "reader: received " ++ show b2
  putMVar done ()

pipe :: IO (CInt, CInt)
pipe = do
  allocaArray 2 $ \ fds -> do
    c_pipe fds
    rd <- peekElemOff fds 0
    wr <- peekElemOff fds 1
    return (rd, wr)

main :: IO ()
main = do
  (readFd, writeFd) <- pipe
  setNonBlocking readFd
--  print (readFd, writeFd)

  done <- newEmptyMVar :: IO (MVar ())
  forkIO $ reader readFd done

  threadDelay 1000000 -- let the other thread reach its blocking state

  alloca $ \ p -> do
    putStrLn "main: writing to pipe"
    poke p 42
    c_write writeFd p 1
    threadDelay 1000000
    poke p 43
    c_write writeFd p 1

  takeMVar done
