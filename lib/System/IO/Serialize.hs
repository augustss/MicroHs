-- Copyright 2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.Serialize(
  hSerialize, hDeserialize,
  writeSerialized, writeSerializedCompressed,
  readSerialized, readSerializedH, readSerializedBS,
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives(Ptr)
import Data.ByteString(ByteString)
import System.IO
import System.IO.Internal
import System.IO.StringHandle(withByteStringHandle)

primHSerialize   :: forall a . Ptr BFILE -> a -> IO ()
primHSerialize    = _primitive "IO.serialize"
primHDeserialize :: forall a . Ptr BFILE -> IO a
primHDeserialize  = _primitive "IO.deserialize"

hSerialize   :: forall a . Handle -> a -> IO ()
hSerialize h a = withHandleWr h $ \ p -> primHSerialize p a

hDeserialize :: forall a . Handle -> IO a
hDeserialize h = withHandleRd h primHDeserialize

writeSerialized :: forall a . FilePath -> a -> IO ()
writeSerialized p s = do
  h <- openBinaryFile p WriteMode
  hSerialize h s
  hClose h

foreign import ccall "add_lz77_compressor" c_add_lz77_compressor :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall "add_lz77_decompressor" c_add_lz77_decompressor :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall "add_lzma_compressor" c_add_lzma_compressor :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall "add_lzma_decompressor" c_add_lzma_decompressor :: Ptr BFILE -> IO (Ptr BFILE)

writeSerializedCompressed :: forall a . FilePath -> a -> IO ()
writeSerializedCompressed p s = do
  h <- openBinaryFile p WriteMode
  hPutChar h 'q'                               -- indicate compressed
  h' <- addTransducer c_add_lzma_compressor h
  hSerialize h' s
  hClose h'

-- Read compressed or uncompressed
readSerialized :: FilePath -> IO a
readSerialized p = openBinaryFile p ReadMode >>= readSerializedH

readSerializedBS :: ByteString -> IO a
readSerializedBS bs = withByteStringHandle bs readSerializedH

readSerializedH :: Handle -> IO a
readSerializedH h = do
  c <- hLookAhead h
  h' <- if c == 'q' then do                    -- compressed?
          hGetChar h   -- get rid of the 'q'
          addTransducer c_add_lzma_decompressor h
        else
          return h
  a <- hDeserialize h'
  hClose h'
  return a
