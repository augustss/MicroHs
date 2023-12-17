module Data.IORef(module Data.IORef) where
import Primitives
import Data.Eq

-- An IORef is represented as an IOArray with a single element.
newtype IORef a = R (IOArray a)

instance forall a . Eq (IORef a) where
  R x == R y  =  primArrEQ x y

newIORef :: forall a . a -> IO (IORef a)
newIORef a = primArrAlloc 1 a `primBind` \ p -> primReturn (R p)

readIORef :: forall a . IORef a -> IO a
readIORef (R p) = primArrRead p 0

writeIORef :: forall a . IORef a -> a -> IO ()
writeIORef (R p) a = primArrWrite p 0 a
