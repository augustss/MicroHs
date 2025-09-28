module Foreign.Ptr(
  Ptr,
  nullPtr,
  castPtr,
  plusPtr,
  alignPtr,
  minusPtr,
  FunPtr,
  nullFunPtr,
  castFunPtr,
  castFunPtrToPtr,
  castPtrToFunPtr,
  freeHaskellFunPtr,
  IntPtr,
  ptrToIntPtr,
  intPtrToPtr,
  WordPtr,
  ptrToWordPtr,
  wordPtrToPtr,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Bits.Base
import Data.Bool_Type
import Data.Bounded
import Data.Coerce
import Data.Enum
import Data.Eq
import Data.Function
import Data.Integral
import Data.Num
import Data.Ord
import Data.Real
import {-# SOURCE #-} Data.Typeable
import Data.Word.Word ()
import Numeric.Show(showHex)
import Text.Show

instance Eq (Ptr a) where
  p == q  =  primPtrToWord p == primPtrToWord q

instance Ord (Ptr a) where
  p `compare` q  =  primPtrToWord p `compare` primPtrToWord q
  p <  q  =  primPtrToWord p <  primPtrToWord q
  p <= q  =  primPtrToWord p <= primPtrToWord q
  p >  q  =  primPtrToWord p >  primPtrToWord q
  p >= q  =  primPtrToWord p >= primPtrToWord q

instance Show (Ptr a) where
  showsPrec _ p = showString "0x" . showHex (primPtrToWord p)

nullPtr :: forall a . Ptr a
nullPtr = primIntToPtr (0::Int)

castPtr :: forall a b . Ptr a -> Ptr b
castPtr = primUnsafeCoerce

plusPtr :: forall a b . Ptr a -> Int -> Ptr b
plusPtr p i = primIntToPtr (primPtrToInt p `primIntAdd` i)

alignPtr :: forall a . Ptr a -> Int -> Ptr a
alignPtr p i =
  case primIntRem (primPtrToInt p) i of
    (0::Int) -> p
    n -> plusPtr p (i `primIntSub` n)

minusPtr :: forall a b . Ptr a -> Ptr b -> Int
minusPtr p q = primPtrToInt p `primIntSub` primPtrToInt q

-------

instance Show (FunPtr a) where
  showsPrec _ p = showString "0x" . showHex (primFunPtrToWord p)

instance Eq (FunPtr a) where
  p == q  =  primFunPtrToWord p == primFunPtrToWord q

instance Ord (FunPtr a) where
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

freeHaskellFunPtr :: forall a . FunPtr a -> IO ()
freeHaskellFunPtr _p = primReturn ()

--------

newtype IntPtr = IntPtr Int
  deriving newtype (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show, Bits, FiniteBits)

ptrToIntPtr :: forall a . Ptr a -> IntPtr
ptrToIntPtr p = IntPtr (primPtrToInt p)

intPtrToPtr :: forall a . IntPtr -> Ptr a
intPtrToPtr (IntPtr i) = primIntToPtr i

newtype WordPtr = WordPtr Word
  deriving newtype (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show, Bits, FiniteBits)

ptrToWordPtr :: forall a . Ptr a -> WordPtr
ptrToWordPtr p = WordPtr (primPtrToWord p)

wordPtrToPtr :: forall a . WordPtr -> Ptr a
wordPtrToPtr (WordPtr w) = primWordToPtr w
