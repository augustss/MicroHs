module NonBlockIO where

import Control.Concurrent
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.FD

foreign import ccall "sys/socket.h socket"     c_socket     :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h setsockopt" c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h bind"       c_bind       :: CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "sys/socket.h listen"     c_listen     :: CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h accept"     c_accept     :: CInt -> Ptr Word8 -> Ptr CInt -> IO CInt
foreign import ccall "htons"                   htons        :: Word16 -> Word16
foreign import ccall "fcntl.h fcntl"           c_fcntl_setfl :: CInt -> CInt -> CInt -> IO CInt

-- Platform constants pulled from the system headers instead of hardcoded Linux values.
foreign import capi "sys/socket.h value AF_INET"      aFINET      :: CInt
foreign import capi "sys/socket.h value SOL_SOCKET"   sOLSOCKET   :: CInt
foreign import capi "sys/socket.h value SOCK_STREAM"  sOCKSTREAM  :: CInt
foreign import capi "sys/socket.h value SO_REUSEADDR" sOREUSEADDR :: CInt
foreign import capi "fcntl.h value F_SETFL"           fSETFL      :: CInt
foreign import capi "fcntl.h value O_NONBLOCK"        oNONBLOCK   :: CInt

-- ismacos is provided by the MHS runtime (returns non-zero on macOS/Darwin).
foreign import ccall "ismacos" ismacos :: IO CInt

setNonBlocking :: CInt -> IO ()
setNonBlocking fd = do
  c_fcntl_setfl fd fSETFL oNONBLOCK
  return ()

-- open and configure a socket. Important that it is set to O_NONBLOCK.
openServerSocket :: Word16 -> IO CInt
openServerSocket port = do
  fd <- c_socket aFINET sOCKSTREAM 0
  setNonBlocking fd

  alloca $ \p -> do
    poke p (1 :: CInt)
    c_setsockopt fd sOLSOCKET sOREUSEADDR p 4
    return ()

  -- Build struct sockaddr_in (16 bytes).
  -- On Linux:  [sin_family: uint16_t at 0][sin_port: uint16_t at 2]...
  -- On macOS:  [sin_len: uint8_t at 0][sin_family: uint8_t at 1][sin_port: uint16_t at 2]...
  mac <- ismacos
  allocaBytes 16 $ \p -> do
    mapM_ (\i -> pokeByteOff p i (0 :: Word8)) [0..15]
    if mac /= 0
      then do
        pokeByteOff p 0 (16 :: Word8)  -- sin_len = sizeof(sockaddr_in)
        pokeByteOff p 1 (2  :: Word8)  -- sin_family = AF_INET
      else
        pokeByteOff p 0 (2 :: Word8)   -- sin_family low byte (LE uint16_t)
    pokeByteOff p 2 (fromIntegral (htons port `div` 256) :: Word8)
    pokeByteOff p 3 (fromIntegral (htons port `mod` 256) :: Word8)
    c_bind fd p 16
    return ()

  c_listen fd 1

  return fd

blockOnAccept :: CInt -> IO ()
blockOnAccept fd = do
  r <- c_accept fd nullPtr nullPtr
  if r /= -1 then return () else do -- O_NONBLOCK, returns immediately when no client
    errno <- getErrno
    if errno == eAGAIN || errno == eWOULDBLOCK
      then do
        waitForReadFD (fromIntegral fd) -- block via epoll/kqueue, letting other threads run
        blockOnAccept fd -- woken up by epoll/kqueue, retry accept
      else throwErrno "accept"

server :: IO ()
server = do
  fd <- openServerSocket 19876
  putStrLn "server: listening, blocking on accept"
  blockOnAccept fd

main :: IO ()
main = do
  forkIO server
  yield -- let the server thread run and print its message
  let tick = putStrLn "tick" >> threadDelay 1000000
  tick
  tick
  tick
