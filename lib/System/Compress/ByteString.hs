module System.Compress.ByteString(
  decompressLZ,
  decompressRLE,
  decompressLZRLE,
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives(primForeignPtrToPtr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Function
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.IO.Internal
import System.IO.Unsafe

type PBFILE = Ptr BFILE
type Transducer = PBFILE -> IO PBFILE
foreign import ccall "openb_wr_mem"          c_openb_wr_mem          :: IO PBFILE
foreign import ccall "openb_rd_mem"          c_openb_rd_mem          :: CString -> Int -> IO PBFILE
foreign import ccall "add_lz77_compressor"   c_add_lz77_compressor   :: Transducer
foreign import ccall "add_lz77_decompressor" c_add_lz77_decompressor :: Transducer
foreign import ccall "add_rle_compressor"    c_add_rle_compressor    :: Transducer
foreign import ccall "add_rle_decompressor"  c_add_rle_decompressor  :: Transducer
foreign import ccall "add_bwt_compressor"    c_add_bwt_compressor    :: Transducer
foreign import ccall "add_bwt_decompressor"  c_add_bwt_decompressor  :: Transducer
foreign import ccall "putb"                  c_putb                  :: Int -> PBFILE -> IO ()
foreign import ccall "getb"                  c_getb                  :: PBFILE -> IO Int
foreign import ccall "get_mem"               c_get_mem               :: PBFILE -> Ptr CString -> Ptr Int -> IO ()
foreign import ccall "closeb"                c_close                 :: PBFILE -> IO ()
foreign import ccall "flushb"                c_flush                 :: PBFILE -> IO ()

withGetTransducerBS :: Transducer -> BS.ByteString -> BS.ByteString
withGetTransducerBS trans bs = unsafePerformIO $ do
  BS.unsafeUseAsCStringLen bs $ \ (ptr, len) -> do
  bf <- c_openb_rd_mem ptr len                 -- open it for reading
  cbf <- trans bf                              -- and add transducer (e.g., decompressor)
  h <- mkHandle "withGetTransducer" cbf HRead
  seq bs (return ())
  BS.hGetContents h

decompressLZ :: BS.ByteString -> BS.ByteString
decompressLZ = withGetTransducerBS c_add_lz77_decompressor

decompressRLE :: BS.ByteString -> BS.ByteString
decompressRLE = withGetTransducerBS c_add_rle_decompressor

decompressLZRLE :: BS.ByteString -> BS.ByteString
decompressLZRLE = withGetTransducerBS (c_add_rle_decompressor <=< c_add_lz77_decompressor)
