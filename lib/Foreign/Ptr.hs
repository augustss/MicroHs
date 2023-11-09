module Foreign.Ptr(Ptr, nullPtr) where
import Primitives
import Prelude
import Data.Word

instance forall a . Eq (Ptr a) where
  p == q  =  primPtrToWord p == primPtrToWord q

instance forall a . Show (Ptr a) where
  show p = "PTR#" ++ show (primPtrToWord p)

nullPtr :: forall a . Ptr a
nullPtr = primWordToPtr 0
