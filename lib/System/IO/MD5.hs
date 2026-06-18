-- Copyright 2023-2026 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.MD5(MD5CheckSum, md5File, md5Handle, md5String, md5Combine) where
import qualified Prelude(); import MiniPrelude
import Primitives(primPerformIO)
import Control.DeepSeq.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce
import Data.Word(Word8)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array(withArrayLen)
import Foreign.Marshal.Utils(copyBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.IO.Internal

foreign import ccall "md5BFILE"  c_md5BFILE  :: Ptr BFILE        -> Ptr MD5CheckSum -> IO ()
foreign import ccall "md5String" c_md5String :: CString          -> Ptr MD5CheckSum -> IO ()
foreign import ccall "md5Array"  c_md5Array  :: Ptr MD5CheckSum  -> Ptr MD5CheckSum -> Int -> IO ()

newtype MD5CheckSum = MD5 BS.ByteString  -- 16 bytes
  deriving (Eq, Ord)

instance Show MD5CheckSum where
  showsPrec p (MD5 bs) = showParen (p > 10) $ showString "MD5 " . showString (concatMap hex (BS.unpack bs))
    where hex b = [intToDigit q, intToDigit r] where (q, r) = quotRem (fromIntegral b) 16

instance NFData MD5CheckSum where
  rnf (MD5 bs) = seq bs ()

instance Storable MD5CheckSum where
  sizeOf _ = md5Len
  alignment _ = 1
  peek p = MD5 <$> BS.packCStringLen (castPtr p, md5Len)
  poke p (MD5 bs) = BS.unsafeUseAsCString bs $ \ cp -> copyBytes p (castPtr cp) md5Len

md5Len :: Int
md5Len = 16   -- The MD5 checksum is 16 bytes

chksum :: (Ptr MD5CheckSum -> IO ()) -> IO MD5CheckSum
chksum fn =
  alloca $ \ buf -> do
    fn buf
    peek buf

md5String :: String -> MD5CheckSum
md5String s = primPerformIO $ withCAString s $ chksum . c_md5String

md5Handle :: Handle -> IO MD5CheckSum
md5Handle h = withHandleRd h $ chksum . c_md5BFILE

md5File :: FilePath -> IO (Maybe MD5CheckSum)
md5File fn = do
  mh <- openFileM fn ReadMode
  case mh of
    Nothing -> return Nothing
    Just h -> do
      cs <- md5Handle h
      hClose h
      return (Just cs)

md5Combine :: [MD5CheckSum] -> MD5CheckSum
md5Combine [] = error "md5Combine: empty"
md5Combine [m] = m
md5Combine ms = primPerformIO $
  withArrayLen ms $ \ l a ->
    chksum $ \ w -> c_md5Array a w (l * md5Len)
