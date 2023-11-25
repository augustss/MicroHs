-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.MD5(MD5CheckSum, md5File, md5Handle) where
import Primitives(primUnsafeCoerce)
import Prelude
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr

foreign import ccall "md5File" c_md5File :: Handle -> CString -> IO ()

newtype MD5CheckSum = MD5 [Word]  -- actually returns 16 bytes

instance Eq MD5CheckSum where
  MD5 a == MD5 b  =  a == b

instance Show MD5CheckSum where
  show (MD5 ws) = "MD5" ++ show ws

md5Len :: Int
md5Len = 16   -- The MD5 checksum is 16 bytes, use one byte / word

md5Handle :: Handle -> IO MD5CheckSum
md5Handle h = do
  buf <- mallocBytes md5Len
  c_md5File h buf
  md5 <- peekCAStringLen (buf, md5Len)
  let
    wmd5 :: [Word]
    wmd5 = primUnsafeCoerce md5
  free buf
  return (MD5 wmd5)

md5File :: FilePath -> IO (Maybe MD5CheckSum)
md5File fn = do
  mh <- openFileM fn ReadMode
  case mh of
    Nothing -> return Nothing
    Just h -> do
      cs <- md5Handle h
      hClose h
      return (Just cs)
