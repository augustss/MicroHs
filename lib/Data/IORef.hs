module Data.IORef(
  IORef,
  newIORef,
  readIORef,
  writeIORef,
  modifyIORef,
  modifyIORef',
  atomicModifyIORef,
  atomicModifyIORef',
  atomicWriteIORef,
  mkWeakIORef,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Eq
import Data.Maybe_Type
import {-# SOURCE #-} Data.Typeable
import System.Mem.Weak

-- An IORef is represented as an IOArray with a single element.
newtype IORef a = R (IOArray a)
  deriving (Typeable)

instance Eq (IORef a) where
  R x == R y  =  primArrEQ x y

newIORef :: forall a . a -> IO (IORef a)
newIORef a = primArrAlloc 1 a `primBind` \ p -> primReturn (R p)

readIORef :: forall a . IORef a -> IO a
readIORef (R p) = primArrRead p 0

writeIORef :: forall a . IORef a -> a -> IO ()
writeIORef (R p) a = primArrWrite p 0 a

modifyIORef :: forall a . IORef a -> (a -> a) -> IO ()
modifyIORef (R p) f = primArrRead p 0 `primBind` \ a -> primArrWrite p 0 (f a)

modifyIORef' :: forall a . IORef a -> (a -> a) -> IO ()
modifyIORef' (R p) f = primArrRead p 0 `primBind` \ a -> let a' = f a in primSeq a' (primArrWrite p 0 a')

mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r fin = mkWeakPtr r (Just fin)

-- XXX WRONG
atomicModifyIORef :: forall a b . IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef r f = readIORef r `primBind` \ a -> let (a', b) = f a in writeIORef r a' `primThen` primReturn b

-- XXX WRONG
atomicModifyIORef' :: forall a b . IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' r f = readIORef r `primBind` \ a -> let (a', b) = f a in primSeq a' (writeIORef r a') `primThen` primSeq b (primReturn b)

atomicWriteIORef :: forall a . IORef a -> a -> IO ()
atomicWriteIORef = writeIORef
