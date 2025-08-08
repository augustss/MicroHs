module System.Compress(
  compress, decompress,
  compressRLE, decompressRLE,
  compressBWT, decompressBWT,
  compressBWTRLE, decompressBWTRLE,
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives(primForeignPtrToPtr)
import qualified Data.ByteString.Internal as BS
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

withPutTransducer :: Transducer -> [Char] -> [Char]
withPutTransducer trans file = unsafePerformIO $ do
  bf <- c_openb_wr_mem          -- create a buffer
  cbf <- trans bf               -- and add transducer (e.g., a compressor)
  mapM_ (flip c_putb cbf . ord) file -- copy all the bytes
  c_flush cbf                   -- do compression and write to buffer
  with nullPtr $ \ bufp ->
    with 0 $ \ lenp -> do
      c_get_mem bf bufp lenp    -- get buffer and length
      buf <- peek bufp
      len <- peek lenp
      res <- peekCAStringLen (buf, len) -- encode as a string
      free buf                  -- free owned memory
      c_close cbf               -- and close everything
      return res

withGetTransducer :: Transducer -> [Char] -> [Char]
withGetTransducer trans file = unsafePerformIO $ do
  (ptr, len) <- newCAStringLen file            -- make memory buffer
  bf <- c_openb_rd_mem ptr len                 -- open it for reading
  cbf <- trans bf                              -- and add transducer (e.g., decompressor)
  h <- mkHandle "withGetTransducer" cbf HRead
  cs <- hGetContents h                         -- get contents
  seq (length cs) (return ())                  -- force it all so ptr is no longer in use
  hClose h  -- XXX why?
  return cs

compress :: [Char] -> [Char]
compress = withPutTransducer c_add_lz77_compressor

decompress :: [Char] -> [Char]
decompress = withGetTransducer c_add_lz77_decompressor

compressRLE :: [Char] -> [Char]
compressRLE = withPutTransducer c_add_rle_compressor

decompressRLE :: [Char] -> [Char]
decompressRLE = withGetTransducer c_add_rle_decompressor

compressBWT :: [Char] -> [Char]
compressBWT = withPutTransducer c_add_bwt_compressor

decompressBWT :: [Char] -> [Char]
decompressBWT = withGetTransducer c_add_bwt_decompressor

compressBWTRLE :: [Char] -> [Char]
compressBWTRLE = withPutTransducer (c_add_bwt_compressor <=< c_add_rle_compressor <=< c_add_lz77_compressor)

decompressBWTRLE :: [Char] -> [Char]
decompressBWTRLE = withGetTransducer (c_add_bwt_decompressor <=< c_add_rle_decompressor <=< c_add_lz77_decompressor)

{-
main :: IO ()
main = do
  putStrLn "compress"
  haa <- openBinaryFile "aa" ReadMode
  aa <- hGetContents haa
  let bb = compressBWTRLE aa
  hbb <- openBinaryFile "bb" WriteMode
  hPutStr hbb bb
  hClose hbb
  hClose haa
  putStrLn "decompress"
  hbb' <- openBinaryFile "bb" ReadMode
  bb' <- hGetContents hbb'
  let aa' = decompressBWTRLE bb'
  hcc <- openBinaryFile "cc" WriteMode
  hPutStr hcc aa'
  hClose hbb'
  hClose hcc
-}
