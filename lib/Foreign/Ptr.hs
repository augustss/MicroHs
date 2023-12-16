module Foreign.Ptr(module Foreign.Ptr, Ptr) where
import Primitives
import Data.Word
import Data.Eq
import Data.Function
import Text.Show

instance forall a . Eq (Ptr a) where
  p == q  =  primPtrEQ p q

instance forall a . Show (Ptr a) where
  showsPrec _ p = showString "PTR#" . showsPrec 0 (primPtrToWord p)

nullPtr :: forall a . Ptr a
nullPtr = primWordToPtr 0

castPtr :: forall a b . Ptr a -> Ptr b
castPtr = primUnsafeCoerce

plusPtr :: forall a b . Ptr a -> Int -> Ptr b
plusPtr p i = primIntToPtr (primPtrToInt p `primIntAdd` i)

minusPtr :: forall a b . Ptr a -> Ptr b -> Int
minusPtr p q = primPtrToInt p `primIntSub` primPtrToInt q
