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
import Data.Text.Internal
import Data.Text.Encoding
import System.IO.Base(Handle, IOMode(..), hClose, openBinaryFile, stdin, stdout, withBinaryFile)

readFile :: FilePath -> IO Text
readFile f = do
  h <- openBinaryFile f ReadMode
  hGetContents h

writeFile :: FilePath -> Text -> IO ()
writeFile f t = withBinaryFile f WriteMode $ \ h -> hPutStr h t

appendFile :: FilePath -> Text -> IO ()
appendFile f t = withBinaryFile f AppendMode $ \ h -> hPutStr h t

hGetContents :: Handle -> IO Text
hGetContents h = do
  bs <- BS.hGetContents h
  evaluate (decodeUtf8 bs)

hGetChunk :: Handle -> IO Text
hGetChunk = hGetContents -- XXX: read chunk from buffer

hGetLine :: Handle -> IO Text
hGetLine h = do
  bs <- BS.hGetLine h
  evaluate (decodeUtf8 bs)

hPutStr :: Handle -> Text -> IO ()
hPutStr h (T bs) = BS.hPutStr h bs

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
