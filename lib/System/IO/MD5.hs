module System.IO.MD5(md5file) where
import Primitives(primUnsafeCoerce)
import Prelude
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr

foreign import ccall "md5File" c_md5File :: Handle -> CString -> IO ()

md5Len :: Int
md5Len = 16   -- The MD5 checksum is 16 bytes, use one byte / word

md5file :: FilePath -> IO (Maybe [Word])  -- actually returns 16 bytes
md5file fn = do
  mh <- openFileM fn ReadMode
  case mh of
    Nothing -> return Nothing
    Just h -> do
      buf <- mallocBytes md5Len
      c_md5File h buf
      md5 <- peekCAStringLen (buf, md5Len)
      let
        wmd5 :: [Word]
        wmd5 = primUnsafeCoerce md5
      free buf
      hClose h
      return (Just wmd5)
