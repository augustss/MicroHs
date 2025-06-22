module ForeignC(main) where
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "sys/errno.h &errno" cerrno :: IO (Ptr CInt)
foreign import ccall "unistd.h getpid" getpid :: IO CInt
foreign import capi  "value 3+4" seven :: CInt

main :: IO ()
main = do
  CInt pid <- getpid
  print (pid /= 0)
  p <- cerrno
  CInt e <- peek p
  print e
  print (let CInt x = seven in x)
