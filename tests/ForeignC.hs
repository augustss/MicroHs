module ForeignC(main) where
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "errno.h &errno" cerrno :: IO (Ptr CInt)
foreign import ccall "unistd.h getpid" getpid :: IO CInt
foreign import capi  "value 3+4" seven :: CInt
foreign import ccall labs :: CLong -> CLong

main :: IO ()
main = do
  let CLong r = labs (-33)
  print r
  CInt pid <- getpid
  print (pid /= 0)
  p <- cerrno
  CInt e <- peek p
  print e
  print (let CInt x = seven in x)
