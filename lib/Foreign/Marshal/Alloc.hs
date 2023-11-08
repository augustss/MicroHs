module Foreign.Marshal.Alloc(free) where
import Primitives

free :: forall a . Ptr a -> IO ()
free = primFree
