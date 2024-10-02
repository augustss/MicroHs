module System.Compress(compress) where
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
import System.IO.Internal
import System.IO.Unsafe

foreign import ccall "lz77c" c_lz77c :: CString -> CSize -> Ptr CString -> IO CSize

{-
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
-}

type PBFILE = Ptr BFILE
foreign import ccall "openb_wr_buf" c_openb_wr_buf :: IO PBFILE
foreign import ccall "openb_rd_buf" c_openb_rd_buf :: Ptr Char -> Int -> IO PBFILE
foreign import ccall "add_lz77_compressor" c_add_lz77_compressor :: PBFILE -> IO PBFILE
foreign import ccall "add_lz77_decompressor" c_add_lz77_decompressor :: PBFILE -> IO PBFILE
foreign import ccall "putb" c_putb :: Int -> PBFILE -> IO ()
foreign import ccall "getb" c_getb :: PBFILE -> IO Int
foreign import ccall "get_buf" c_get_buf :: PBFILE -> Ptr (Ptr Char) -> Ptr Int -> IO ()
foreign import ccall "closeb" c_close :: PBFILE -> IO ()
foreign import ccall "flushb" c_flush :: PBFILE -> IO ()

withPutTransducer :: (PBFILE -> IO PBFILE) -> [Char] -> [Char]
withPutTransducer trans file = unsafePerformIO $ do
  bf <- c_openb_wr_buf          -- create a buffer
  cbf <- trans bf               -- and add transducer (e.g., a compressor)
  mapM_ (flip c_putb cbf . ord) file -- copy all the bytes
  c_flush cbf                   -- do compression and write to buffer
  with nullPtr $ \ bufp ->
    with 0 $ \ lenp -> do
      c_get_buf bf bufp lenp    -- get buffer and length
      buf <- peek bufp
      len <- peek lenp
      res <- peekCAStringLen (buf, len) -- encode as a string
      free buf                  -- free owned memory
      c_close cbf               -- and close everything
      return res

withGetTransducer :: (PBFILE -> IO PBFILE) -> [Char] -> [Char]
withGetTransducer trans file = unsafePerformIO $ do
  (ptr, len) <- newCAStringLen file            -- make memory buffer
  bf <- c_openb_rd_buf ptr len                 -- open it for reading
  cbf <- trans bf                              -- and add transducer (e.g., decompressor)
  h <- mkHandle "withGetTransducer" cbf HRead
  cs <- hGetContents h                         -- get contents
  seq (length cs) (return ())                  -- force it all so ptr is no longer in use
  hClose h
  return cs

compress :: [Char] -> [Char]
compress = withPutTransducer c_add_lz77_compressor

decompress :: [Char] -> [Char]
decompress = withGetTransducer c_add_lz77_decompressor

{-
main :: IO ()
main = do
  putStrLn "compress"
  haa <- openBinaryFile "aa" ReadMode
  aa <- hGetContents haa
  let bb = compress' aa
  hbb <- openBinaryFile "bb" WriteMode
  hPutStr hbb bb
  hClose hbb
  hClose haa
  putStrLn "decompress"
  hbb' <- openBinaryFile "bb" ReadMode
  bb' <- hGetContents hbb'
  let aa' = decompress bb'
  hcc <- openBinaryFile "cc" WriteMode
  hPutStr hcc aa'
  hClose hbb'
  hClose hcc
-}
