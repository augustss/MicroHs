module NonBlockIO where

import Control.Concurrent
import Control.Concurrent.MVar
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
foreign import ccall "sys/socket.h connect"   c_connect    :: CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "htons"                   htons        :: Word16 -> Word16
foreign import ccall "fcntl.h fcntl"           c_fcntl_setfl :: CInt -> CInt -> CInt -> IO CInt

-- Platform constants.
--
-- I think an alternative is to hardcode the values in this file rather than pulling them in
-- like this, but conditionally choose the mac or linux variants with `ismacos`. Some of these
-- constants differ on the two platforms, e.g. oNONBLOCK. I have no clue if Windows have their
-- own, third, variant.
foreign import capi "sys/socket.h value AF_INET"      aFINET      :: CInt
foreign import capi "sys/socket.h value SOL_SOCKET"   sOLSOCKET   :: CInt
foreign import capi "sys/socket.h value SOCK_STREAM"  sOCKSTREAM  :: CInt
foreign import capi "sys/socket.h value SO_REUSEADDR" sOREUSEADDR :: CInt
foreign import capi "fcntl.h value F_SETFL"           fSETFL      :: CInt
foreign import capi "fcntl.h value O_NONBLOCK"        oNONBLOCK   :: CInt

-- ismacos is provided by the MHS runtime (returns non-zero on macOS/Darwin).
foreign import ccall "ismacos" ismacos :: IO CInt

-- set a file descriptor to non-blocking mode
setNonBlocking :: CInt -> IO ()
setNonBlocking fd = do
  c_fcntl_setfl fd fSETFL oNONBLOCK
  return ()

-- Build a struct sockaddr_in (16 bytes) and pass it to the given action.
-- layout differ slightly on the two platforms, whereby we have a test on whether
-- we run on macos or linux,
--
-- On Linux:  [sin_family: uint16_t at 0][sin_port: uint16_t at 2][sin_addr: 4 bytes at 4]...
-- On macOS:  [sin_len: uint8_t at 0][sin_family: uint8_t at 1][sin_port: uint16_t at 2][sin_addr: 4 bytes at 4]...
withSockAddr :: Word16 -> (Word8, Word8, Word8, Word8) -> (Ptr Word8 -> IO a) -> IO a
withSockAddr port (a0, a1, a2, a3) action = do
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
    pokeByteOff p 4 a0
    pokeByteOff p 5 a1
    pokeByteOff p 6 a2
    pokeByteOff p 7 a3
    action p

-- open and configure a socket. Important that it is set to O_NONBLOCK.
openServerSocket :: Word16 -> IO CInt
openServerSocket port = do
  -- create nonblocking socket
  fd <- c_socket aFINET sOCKSTREAM 0
  setNonBlocking fd

  alloca $ \p -> do
    poke p (1 :: CInt)
    c_setsockopt fd sOLSOCKET sOREUSEADDR p 4
    return ()

  -- configure the SockAddr stuff
  withSockAddr port (0, 0, 0, 0) $ \p ->
    c_bind fd p 16

  -- enable listening mode
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
        blockOnAccept fd -- when we are woken up here, the socket is ready and the next
                         -- accept try should work immediately.
      else throwErrno "accept"

serverPort :: Word16
serverPort = 19876

-- runs in its own green thread, blocking on IO until someone connects to it
server :: MVar () -> IO ()
server mv = do
  fd <- openServerSocket serverPort
  putStrLn "server: listening, blocking on accept"
  blockOnAccept fd
  _ <- takeMVar mv
  putStrLn "accepted an incoming connection"

connectToServer :: Word16 -> IO ()
connectToServer port = do
  fd <- c_socket aFINET sOCKSTREAM 0
  withSockAddr serverPort (127, 0, 0, 1) $ \p ->
    c_connect fd p 16
  return ()

-- main thread
--
-- forks a green thread running the server, which blocks
-- then, three heartbeats are emitted
-- after which we connect to the server. Both the server and this
-- green thread will issue a print to indicate success, but we use
-- an MVar to guarantee a specific order of their outputs. If the
-- main thread terminates before the server thread prints, it will
-- never print.
main :: IO ()
main = do
  mv <- newEmptyMVar :: IO (MVar ())
  forkIO $ server mv
  yield -- let the server thread run and print its message

  let tick = putStrLn "tick" >> threadDelay 1000000
  tick
  tick
  tick

  connectToServer serverPort
  putMVar mv ()
  yield

  putStrLn "client: connected to server"