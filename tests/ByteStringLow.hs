module ByteStringLow where

import Data.ByteString
import Data.ByteString.Internal
import Foreign
import Foreign.C

foreign import ccall "strlen" c_strlen :: Ptr CChar -> IO CSize
foreign import ccall "malloc" c_malloc :: CSize -> IO (Ptr CChar)
foreign import ccall "strcpy" c_strcpy :: Ptr CChar -> Ptr CChar -> IO ()

-- test some low level ugliness
main :: IO ()
main = do
  let s = "abc" :: ByteString
  print s
  xbs <- useAsCString s $ \ p -> do
    -- p is a pointer to the C string "abc"
    n <- c_strlen p
    q <- c_malloc (n+1)
    c_strcpy q p
    grabCString q
  print xbs

  -- Check that NUL terminates the C string
  let s = "a\NULbc" :: ByteString
  print s
  xbs <- useAsCString s $ \ p -> do
    -- p is a pointer to the C string "a"
    n <- c_strlen p
    q <- c_malloc (n+1)
    c_strcpy q p
    grabCString q
  print xbs
