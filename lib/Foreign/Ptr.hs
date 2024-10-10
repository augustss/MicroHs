module Foreign.Ptr(
  module Foreign.Ptr,
  Ptr,
  FunPtr,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Word
import Data.Eq
import Data.Function
import Data.Ord
import Numeric.Show(showHex)
import Text.Show

instance forall a . Eq (Ptr a) where
  p == q  =  primPtrToWord p == primPtrToWord q

instance forall a . Ord (Ptr a) where
  p `compare` q  =  primPtrToWord p `compare` primPtrToWord q
  p <  q  =  primPtrToWord p <  primPtrToWord q
  p <= q  =  primPtrToWord p <= primPtrToWord q
  p >  q  =  primPtrToWord p >  primPtrToWord q
  p >= q  =  primPtrToWord p >= primPtrToWord q

instance forall a . Show (Ptr a) where
  showsPrec _ p = showString "0x" . showHex (primPtrToWord p)

nullPtr :: forall a . Ptr a
nullPtr = primIntToPtr (0::Int)

castPtr :: forall a b . Ptr a -> Ptr b
castPtr = primUnsafeCoerce

plusPtr :: forall a b . Ptr a -> Int -> Ptr b
plusPtr p i = primIntToPtr (primPtrToInt p `primIntAdd` i)

minusPtr :: forall a b . Ptr a -> Ptr b -> Int
minusPtr p q = primPtrToInt p `primIntSub` primPtrToInt q

-------

instance forall a . Show (FunPtr a) where
  showsPrec _ p = showString "0x" . showHex (primFunPtrToWord p)

instance forall a . Eq (FunPtr a) where
  p == q  =  primFunPtrToWord p == primFunPtrToWord q

instance forall a . Ord (FunPtr a) where
  p `compare` q  =  primFunPtrToWord p `compare` primFunPtrToWord q
  p <  q  =  primFunPtrToWord p <  primFunPtrToWord q
  p <= q  =  primFunPtrToWord p <= primFunPtrToWord q
  p >  q  =  primFunPtrToWord p >  primFunPtrToWord q
  p >= q  =  primFunPtrToWord p >= primFunPtrToWord q

nullFunPtr :: forall a . FunPtr a
nullFunPtr = primIntToFunPtr (0::Int)

castFunPtr :: forall a b . FunPtr a -> FunPtr b
castFunPtr = primUnsafeCoerce

castFunPtrToPtr :: forall a b . FunPtr a -> Ptr b
castFunPtrToPtr = primFunPtrToPtr

castPtrToFunPtr :: forall a b . Ptr a -> FunPtr b
castPtrToFunPtr = primPtrToFunPtr

--------

type IntPtr = Int
type WordPtr = Word
