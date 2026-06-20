module Data.Text.IO (
  -- * File-at-a-time operations
  readFile,
  writeFile,
  appendFile,
  -- * Operations on handles
  hGetContents,
  hGetChunk,
  hGetLine,
  hPutStr,
  hPutStrLn,
  -- * Special cases for standard input and output
  interact,
  getContents,
  getLine,
  putStr,
  putStrLn,
) where

import Prelude hiding (readFile, writeFile)
import qualified Prelude as P
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Text.Internal
import Data.Text(pack, unpack)
import Data.Text.Encoding
--import System.IO.Base(Handle, IOMode(..), hClose, openBinaryFile, stdin, stdout, withBinaryFile)
import System.IO.Base(Handle, IOMode(..), hClose, openFile, stdin, stdout, withFile)
import qualified System.IO.Base as IO

--
-- This is a mess!
-- For fast file operations we want to do IO on the bytestrings directly.
-- But stdin/stdout/stderr and files open with openFile are in UTF-8 mode
-- and there is no way to turn that off.
-- For now, do it the slow way via String.

readFile :: FilePath -> IO Text
readFile f = do
--  h <- openBinaryFile f ReadMode
  h <- openFile f ReadMode
  hGetContents h

writeFile :: FilePath -> Text -> IO ()
writeFile f t = -- withBinaryFile f WriteMode $ \ h -> hPutStr h t
  withFile f WriteMode $ \ h -> hPutStr h t

appendFile :: FilePath -> Text -> IO ()
appendFile f t = -- withBinaryFile f AppendMode $ \ h -> hPutStr h t
  withFile f AppendMode $ \ h -> hPutStr h t

hGetContents :: Handle -> IO Text
hGetContents h = do
{-
--hPutStr h (T bs) = BS.hPutStr h bs  -- need handle to be in raw mode.  It isn't.
  bs <- BS.hGetContents h
  evaluate (decodeUtf8 bs)
-}
  pack <$> IO.hGetContents h

hGetChunk :: Handle -> IO Text
hGetChunk = hGetContents -- XXX: read chunk from buffer

hGetLine :: Handle -> IO Text
hGetLine h = do
{-
--hPutStr h (T bs) = BS.hPutStr h bs  -- need handle to be in raw mode.  It isn't.
  bs <- BS.hGetLine h
  evaluate (decodeUtf8 bs)
-}
  pack <$> IO.hGetLine h

hPutStr :: Handle -> Text -> IO ()
--hPutStr h (T bs) = BS.hPutStr h bs  -- need handle to be in raw mode.  It isn't.
hPutStr h t = IO.hPutStr h (unpack t)

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h t = hPutStr h t >> hPutStr h "\n"

interact :: (Text -> Text) -> IO ()
interact f = getContents >>= putStr . f

getContents :: IO Text
getContents = hGetContents stdin

getLine :: IO Text
getLine = hGetLine stdin

putStr :: Text -> IO ()
putStr = hPutStr stdout

putStrLn :: Text -> IO ()
putStrLn = hPutStrLn stdout
