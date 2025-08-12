module System.IO.Transducers(
  addUTF8, addCRLF, addBuffer, addLZ77, addRLE, addBWT, addBase64,
  ) where
import qualified Prelude(); import MiniPrelude
--import Data.Maybe
import Foreign.Ptr(Ptr)
import System.IO.Base
import System.IO.Internal

foreign import ccall add_utf8              :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_crlf              :: Ptr BFILE -> IO (Ptr BFILE)
--foreign import ccall add_base64            :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_buf        :: Int -> Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_lz77_compressor   :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_lz77_decompressor :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_rle_compressor    :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_rle_decompressor  :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_bwt_compressor    :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_bwt_decompressor  :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_base64_encoder    :: Ptr BFILE -> IO (Ptr BFILE)
foreign import ccall add_base64_decoder    :: Ptr BFILE -> IO (Ptr BFILE)

addTransducerRW :: (Ptr BFILE -> IO (Ptr BFILE)) -> (Ptr BFILE -> IO (Ptr BFILE)) -> Handle -> IO Handle
addTransducerRW rd wr h = do
  hs <- getHandleState h
  case hs of
    HRead  -> addTransducer rd h
    HWrite -> addTransducer wr h
    _      -> error "addTransducerRW: bad R/W mode"

addUTF8 :: Handle -> IO Handle
addUTF8 = addTransducer add_utf8

addCRLF :: Handle -> IO Handle
addCRLF = addTransducer add_crlf

addBuffer :: BufferMode -> Handle -> IO Handle
addBuffer NoBuffering h = return h
addBuffer LineBuffering h = addTransducer (add_buf (-defaultBufSize)) h
addBuffer (BlockBuffering mb) h = addTransducer (add_buf (fromMaybe defaultBufSize mb)) h

defaultBufSize :: Int
defaultBufSize = 512

addLZ77 :: Handle -> IO Handle
addLZ77 = addTransducerRW add_lz77_decompressor add_lz77_compressor

addRLE :: Handle -> IO Handle
addRLE = addTransducerRW add_rle_decompressor add_rle_compressor

addBWT :: Handle -> IO Handle
addBWT = addTransducerRW add_bwt_decompressor add_bwt_compressor

addBase64 :: Handle -> IO Handle
addBase64 = addTransducerRW add_base64_decoder add_base64_encoder
