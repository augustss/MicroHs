module PipeNonBlockIO where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.FD

foreign import ccall "unistd.h pipe"  c_pipe  :: Ptr CInt -> IO CInt
foreign import ccall "unistd.h read"  c_read  :: CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "unistd.h write" c_write :: CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "fcntl.h fcntl"  c_fcntl_setfl :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "fcntl.h value F_SETFL"    fSETFL    :: CInt
foreign import capi "fcntl.h value O_NONBLOCK" oNONBLOCK :: CInt

setNonBlocking :: CInt -> IO ()
setNonBlocking fd = c_fcntl_setfl fd fSETFL oNONBLOCK >> return ()

blockOnRead :: CInt -> IO Word8
blockOnRead fd = alloca $ \p -> go p
  where
    go p = do
      r <- c_read fd p 1
      if r == 1
        then peek p
        else do
          errno <- getErrno
          if errno == eAGAIN || errno == eWOULDBLOCK
            then waitForReadFD (fromIntegral fd) >> go p
            else throwErrno "read"

reader :: CInt -> MVar () -> IO ()
reader fd done = do
  putStrLn "reader: waiting for data"
  b <- blockOnRead fd
  putStrLn $ "reader: received " ++ show b
  putMVar done ()

main :: IO ()
main =
  allocaBytes 8 $ \fds -> do        -- space for two CInt file descriptors
    c_pipe fds
    readFd  <- peekByteOff fds 0 :: IO CInt
    writeFd <- peekByteOff fds 4 :: IO CInt
    setNonBlocking readFd

    done <- newEmptyMVar :: IO (MVar ())
    forkIO $ reader readFd done
    yield                           -- let reader reach waitForReadFD

    threadDelay 1000000 -- let the other thread reach its blocking state
    putStrLn "main: writing to pipe"
    alloca $ \p -> do
      poke p (42 :: Word8)
      c_write writeFd p 1

    takeMVar done
