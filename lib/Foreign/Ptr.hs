module Foreign.Ptr(module Foreign.Ptr, Ptr) where
import Primitives
import Data.Word
import Data.Eq
import Data.Function
import Data.Ord
import Text.Show

instance forall a . Eq (Ptr a) where
  p == q  =  primPtrEQ p q

instance forall a . Ord (Ptr a) where
  p `compare` q  =  primPtrCompare p q

instance forall a . Show (Ptr a) where
  showsPrec _ p = showString "PTR#" . showsPrec 0 (primPtrToWord p)

nullPtr :: forall a . Ptr a
nullPtr = primPtrNull

castPtr :: forall a b . Ptr a -> Ptr b
castPtr = primPtrCast

plusPtr :: forall a b . Ptr a -> Int -> Ptr b
plusPtr = primPtrAdd

minusPtr :: forall a b . Ptr a -> Ptr b -> Int
minusPtr = primPtrSub
