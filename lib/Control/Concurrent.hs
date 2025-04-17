module Control.Concurrent(
  ThreadId,
  forkIO,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Prelude
import Data.Word

newtype ThreadId = ThreadId (Ptr ())

instance Show ThreadId where
  show (ThreadId p) = "ThreadId#" ++ show (primPtrToWord p)

instance Eq ThreadId where
  ThreadId p == ThreadId q  =  primPtrEQ p q

instance Ord ThreadId where
  ThreadId p `compare` ThreadId q = p `compare` q

primForkIO :: IO () -> IO (Ptr ())
primForkIO = primitive "IO.fork"

forkIO :: IO () -> IO ThreadId
forkIO io = ThreadId <$> primForkIO io
