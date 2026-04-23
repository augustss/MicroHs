module NonBlockIO where

import Control.Concurrent
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.FD

foreign import ccall "socket"     c_socket     :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "setsockopt" c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
foreign import ccall "bind"       c_bind       :: CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "listen"     c_listen     :: CInt -> CInt -> IO CInt
foreign import ccall "accept"     c_accept     :: CInt -> Ptr Word8 -> Ptr CInt -> IO CInt
foreign import ccall "htons"      htons        :: Word16 -> Word16

-- some wonderful Linux constants

aFINET :: CInt
aFINET = 2

sOLSOCKET :: CInt
sOLSOCKET = 1

sOCKSTREAM :: CInt
sOCKSTREAM  = 1

sOCKNONBLOCK :: CInt
sOCKNONBLOCK = 0x800

sOREUSEADDR :: CInt
sOREUSEADDR = 2

-- open and configure a socket. Important that it is set to O_NONBLOCK

openServerSocket :: Word16 -> IO CInt
openServerSocket port = do
  fd <- c_socket aFINET (sOCKSTREAM + sOCKNONBLOCK) 0

  alloca $ \p -> do
    poke p (1 :: CInt)
    c_setsockopt fd sOLSOCKET sOREUSEADDR p 4
    return ()

  -- struct sockaddr_in, configuring this to AF_INET
  allocaBytes 16 $ \p -> do
    mapM_ (\i -> pokeByteOff p i (0 :: Word8)) [0..15]
    pokeByteOff p 0 (2 :: Word8)
    pokeByteOff p 2 (fromIntegral (htons port `div` 256) :: Word8)
    pokeByteOff p 3 (fromIntegral (htons port `mod` 256) :: Word8)
    c_bind fd p 16
    return ()

  c_listen fd 1

  return fd

blockOnAccept :: CInt -> IO ()
blockOnAccept fd = do
  r <- c_accept fd nullPtr nullPtr
  if r /= -1 then return () else do -- O_NONBLOCK, now it returns straight away
    errno <- getErrno
    if errno == eAGAIN || errno == eWOULDBLOCK
      then do
        waitForReadFD (fromIntegral fd) -- this makes the runtime block, but using epoll, allowing other green threads to run
        blockOnAccept fd -- when we run this, we were woken up by epoll and the accept call should now work
      else throwErrno "accept"

server :: IO ()
server = do
  fd <- openServerSocket 19876
  putStrLn "server: listening, blocking on accept"
  blockOnAccept fd

main :: IO ()
main = do
  forkIO server
  yield -- yielding, letting the server thread run and printing its message
  let tick = putStrLn "tick" >> threadDelay 1000000
  tick
  tick
  tick
