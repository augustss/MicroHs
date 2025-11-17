-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO(
  module System.IO.Base,
  readIO,
  readLn,
  fixIO, FixIOException,
  Newline(..),
  NewlineMode(..),

  TextEncoding,
  mkTextEncoding, hSetEncoding,
  latin1, utf8, utf8_bom, utf16, utf16le, utf16be, utf32, utf32le, utf32be, localeEncoding, char8,
  SeekMode(..),
  hIsOpen,
  hIsClosed,
  hIsReadable,
  hIsWritable,
  hIsSeekable,
  hIsTerminalDevice,
  hGetEcho,
  hSetEcho,
  hReady,

  ) where
import qualified Prelude()              -- do not import Prelude
import MiniPrelude
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Fix
import Data.Bounded
import Data.IORef
import Mhs.Builtin
import System.IO.Base
import System.IO.Error
import System.IO.Unsafe(unsafeInterleaveIO)
import System.IO_Handle
import Text.Read

readLn :: Read a => IO a
readLn = getLine >>= readIO

readIO :: Read a => String -> IO a
readIO s =
  case (do { (x,t) <- reads s;
             ("","") <- lex t;
             return x }) of
    [x]    -> return x
    []     -> ioError (userError "Prelude.readIO: no parse")
    _      -> ioError (userError "Prelude.readIO: ambiguous parse")

------------------------------------------------

data FixIOException = FixIOException

instance Exception FixIOException

instance Show FixIOException where
  showsPrec _ FixIOException = showString "cyclic evaluation in fixIO"

fixIO :: (a -> IO a) -> IO a
fixIO k = do
    m <- newEmptyMVar
    ans <- unsafeInterleaveIO
             (readMVar m `catch` \ BlockedIndefinitelyOnMVar ->
                                   throwIO FixIOException)
    result <- k ans
    putMVar m result
    return result

instance MonadFix IO where
  mfix = fixIO

data Newline = LF | CRLF
  deriving (Eq, Ord, Show, Read)

data NewlineMode = NewlineMode {inputNL :: Newline, outputNL :: Newline}
  deriving (Eq, Ord, Show, Read)

--------
-- For compatibility
-- This is a complete lie

data TextEncoding = UTF8

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding "UTF-8//ROUNDTRIP" = return UTF8
mkTextEncoding _ = error "unknown text encoding"

-- XXX Always in UTF8 mode
hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding _ _ = return ()

latin1, utf8, utf8_bom, utf16, utf16le, utf16be, utf32, utf32le, utf32be, localeEncoding, char8 :: TextEncoding
latin1 = UTF8
utf8 = UTF8
utf8_bom = UTF8
utf16 = UTF8
utf16le = UTF8
utf16be = UTF8
utf32 = UTF8
utf32le = UTF8
utf32be = UTF8
localeEncoding = UTF8
char8 = UTF8

-------

data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

-------

-- XXX This currently does nothing.
hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice _ = return True

hIsOpen :: Handle -> IO Bool
hIsOpen (Handle _ r _) = do
  s <- readIORef r
  return $
    case s of
      HSemiClosed -> False
      HClosed -> False
      _ -> True

hIsClosed :: Handle -> IO Bool
hIsClosed (Handle _ r _) = do
  s <- readIORef r
  return $
    case s of
      HClosed -> True
      _ -> False

hIsReadable :: Handle -> IO Bool
hIsReadable (Handle _ r _) = do
  s <- readIORef r
  return $
    case s of
      HRead -> True
      HReadWrite -> True
      _ -> False

hIsWritable :: Handle -> IO Bool
hIsWritable (Handle _ r _) = do
  s <- readIORef r
  return $
    case s of
      HWrite -> True
      HReadWrite -> True
      _ -> False

-- XXX This currently does nothing.
hIsSeekable :: Handle -> IO Bool
hIsSeekable _ = return False

-- XXX This currently does nothing.
hGetEcho :: Handle -> IO Bool
hGetEcho _ = return True

-- XXX This currently does nothing.
hSetEcho :: Handle -> Bool -> IO ()
hSetEcho _ _ = return ()

-- XXX This currently does nothing.
hReady :: Handle -> IO Bool
hReady _ = return False
