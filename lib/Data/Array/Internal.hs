module Data.Array.Internal(module Data.Array.Internal, IOVector) where
import qualified Prelude()              -- do not import Prelude
import Primitives

-- data IOVector

newIOVector :: forall a . Int -> a -> IO (IOVector a)
newIOVector = primArrAlloc

sizeIOVector :: forall a . IOVector a -> IO Int
sizeIOVector = primArrSize

readIOVector :: forall a . IOVector a -> Int -> IO a
readIOVector = primArrRead

writeIOVector :: forall a . IOVector a -> Int -> a -> IO ()
writeIOVector = primArrWrite
