module System.Compress {-(compress)-} where
import Prelude(); import MiniPrelude
import Data.Function
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.IO_Handle
import System.IO.Unsafe

foreign import ccall "lz77c" c_lz77c :: CString -> CSize -> Ptr CString -> IO CSize

-- This really ought to be [Word8] -> [Word8]
compress :: String -> String
compress file = unsafePerformIO $ do
  (iptr, ilen) <- newCAStringLen file
  pptr <- new nullPtr
  olen <- c_lz77c iptr (intToCSize ilen) pptr
  optr <- peek pptr
  res <- peekCAStringLen (optr, cSizeToInt olen)
  free iptr
  free optr
  return res

type PBFILE = Ptr BFILE
foreign import ccall "openb_wr_buf" c_openb_wr_buf :: IO PBFILE
foreign import ccall "openb_rd_buf" c_openb_rd_buf :: Ptr Char -> Int -> IO PBFILE
foreign import ccall "add_lz77_compressor" c_add_lz77_compressor :: PBFILE -> IO PBFILE
foreign import ccall "add_lz77_decompressor" c_add_lz77_decompressor :: PBFILE -> IO PBFILE
foreign import ccall "putb" c_putb :: Int -> PBFILE -> IO ()
foreign import ccall "get_buf" c_get_buf :: PBFILE -> Ptr (Ptr Char) -> Ptr Int -> IO ()

withPutTransducer :: (PBFILE -> IO PBFILE) -> [Char] -> [Char]
withPutTransducer trans file = unsafePerformIO $ do
  bf <- c_openb_wr_buf
  cbf <- trans bf
  mapM_ (flip c_putb cbf . ord) file
  with nullPtr $ \ bufp ->
    with 0 $ \ lenp -> do
      c_get_buf bf bufp lenp
      buf <- peek bufp
      len <- peek lenp
      res <- peekCAStringLen (buf, len)
      free buf
      return res

withGetTransducer :: (PBFILE -> IO PBFILE) -> [Char] -> [Char]
withGetTransducer trans file = unsafePerformIO $ do
  (ptr, len) <- newCAStringLen file
  bf <- c_openb_rd_buf ptr len >>= trans
  h <- mkHandle "withGetTransducer" bf
  hGetContents h

compress' :: [Char] -> [Char]
compress' = withPutTransducer c_add_lz77_compressor

decompress :: [Char] -> [Char]
decompress = withGetTransducer c_add_lz77_decompressor

main :: IO ()
main = do
  haa <- openBinaryFile "aa" ReadMode
  putStrLn "AA"
  aa <- hGetContents haa
  putStrLn "BB"
  let bb = compress' aa
  putStrLn "CC"
  hbb <- openBinaryFile "bb" WriteMode
  putStrLn "DD"
  hPutStr hbb bb
  putStrLn "EE"
  hClose hbb
  putStrLn "FF"
  hClose haa
  putStrLn "GG"
