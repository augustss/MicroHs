module Foreign.Ptr(Ptr, nullPtr) where
import Primitives
import Data.Word
import Data.Eq
import Data.Function
import Text.Show

instance forall a . Eq (Ptr a) where
  p == q  =  primPtrToWord p == primPtrToWord q

instance forall a . Show (Ptr a) where
  showsPrec _ p = showString "PTR#" . showsPrec 0 (primPtrToWord p)

nullPtr :: forall a . Ptr a
nullPtr = primWordToPtr 0
