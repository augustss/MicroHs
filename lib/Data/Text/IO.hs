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
import Data.Text
import Data.Text.Encoding
import System.IO.Base(Handle, IOMode(..), hClose, openFile, stdin, stdout)
import qualified System.IO.Base as IO

readFile :: FilePath -> IO Text
readFile f = do
  h <- openFile f ReadMode
  hGetContents h

writeFile :: FilePath -> Text -> IO ()
writeFile f bs = do
  h <- openFile f WriteMode
  hPutStr h bs
  hClose h

appendFile :: FilePath -> Text -> IO ()
appendFile f bs = do
  h <- openFile f AppendMode
  hPutStr h bs
  hClose h

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
hPutStr h t =
  -- BS.hPutStr is "buggy", if the handle is UTF8 encoding mode, there will be a double encoding
  -- BS.hPutStr h (encodeUtf8 t)
  -- For now, go via String
  IO.hPutStr h (unpack t)

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h t = hPutStr h t >> hPutStr h (pack "\n")

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
