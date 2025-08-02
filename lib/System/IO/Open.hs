module System.IO.Open(
  stdin, stdout, stderr,
  openFileM,
  openBinaryFileM,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives(ForeignPtr, IO)
import Control.Monad
import Data.Char
import Data.Function
import Data.Functor
import Data.Maybe
import Foreign.C.String
import Foreign.Ptr
import Mhs.Builtin
import System.IO.Internal

-- Functions returning Handles
-- These can be based on FILE or file descriptors.

primStdin        :: ForeignPtr BFILE
primStdin         = _primitive "IO.stdin"
primStdout       :: ForeignPtr BFILE
primStdout        = _primitive "IO.stdout"
primStderr       :: ForeignPtr BFILE
primStderr        = _primitive "IO.stderr"

data FILE

-- FILE stuff
foreign import ccall "fopen"        c_fopen        :: CString -> CString -> IO (Ptr FILE)
foreign import ccall "add_FILE"     c_add_FILE     :: Ptr FILE           -> IO (Ptr BFILE)
foreign import ccall "add_utf8"     c_add_utf8     :: Ptr BFILE          -> IO (Ptr BFILE)

stdin  :: Handle
stdin  = unsafeHandle primStdin  HRead  "stdin"
stdout :: Handle
stdout = unsafeHandle primStdout HWrite "stdout"
stderr :: Handle
stderr = unsafeHandle primStderr HWrite "stderr"

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
openFileM fn m = do
  mf <- openFILEM fn m
  case mf of
    Nothing -> return Nothing
    Just p -> do { q <- c_add_utf8 =<< c_add_FILE p; Just <$> mkHandle fn q (ioModeToHMode m) }

openBinaryFileM :: String -> IOMode -> IO (Maybe Handle)
openBinaryFileM fn m = do
  mf <- openFILEM fn m
  case mf of
    Nothing -> return Nothing
    Just p -> do { q <- c_add_FILE p; Just <$> mkHandle fn q (ioModeToHMode m) }
