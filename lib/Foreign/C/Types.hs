-- Types used for C FFI.
module Foreign.C.Types(
 CChar(..),  CSChar(..),  CUChar(..),
 CShort(..), CUShort(..),
 CInt(..),   CUInt(..),
 CLong(..),  CULong(..),
 CPtrdiff(..),
 CSize(..),  CSSize(..),
 CLLong(..), CULLong(..),
 CIntPtr(..), CUIntPtr(..),
 CFloat(..), CDouble(..),
 CTime(..),
 intToCSize, cSizeToInt,
 ) where
import Prelude()
import Primitives
import Data.Bool
import Data.Eq
import Data.Int
import Data.Integral
import Data.Num
import Data.Ord
import Data.Real
import Data.Word
import Data.FloatW

-- The MicroHs Word type is the "natural" architecture word size;
-- it is the same as the pointer difference type.
-- And Int is the natural signed word size.
newtype CChar    = CChar    Char
  deriving (Eq, Ord)
newtype CSChar   = CSChar   Int
  deriving (Eq, Ord)
newtype CUChar   = CUChar   Word
  deriving (Eq, Ord)
newtype CShort   = CShort   Int
  deriving (Eq, Ord)
newtype CUShort  = CUShort  Word
  deriving (Eq, Ord)
newtype CInt     = CInt     Int
  deriving (Eq, Ord)
newtype CUInt    = CUInt    Word
  deriving (Eq, Ord)
newtype CLong    = CLong    Int
  deriving (Eq, Ord)
newtype CULong   = CULong   Word
  deriving (Eq, Ord)
newtype CPtrdiff = CPtrdiff Word
  deriving (Eq, Ord)
newtype CSize    = CSize    Word
  deriving (Eq, Ord)
newtype CSSize   = CSSize   Int
  deriving (Eq, Ord)
newtype CLLong   = CLLong   Int
  deriving (Eq, Ord)
newtype CULLong  = CULLong  Word
  deriving (Eq, Ord)
newtype CIntPtr  = CIntPtr  Int
  deriving (Eq, Ord)
newtype CUIntPtr = CUIntPtr Word
  deriving (Eq, Ord)

-- XXX This is really platform specific
newtype CTime = CTime Int
  deriving (Eq, Ord)

-- XXX We really need GND
instance Num CInt where
  CInt x + CInt y = CInt (x + y)
  CInt x - CInt y = CInt (x - y)
  CInt x * CInt y = CInt (x * y)
  fromInteger x = CInt (fromInteger x)
instance Num CLong where
  CLong x + CLong y = CLong (x + y)
  CLong x - CLong y = CLong (x - y)
  CLong x * CLong y = CLong (x * y)
  fromInteger x = CLong (fromInteger x)
instance Integral CLong where
  quotRem (CLong x) (CLong y) = (CLong q, CLong r) where (q, r) = quotRem q r
  toInteger (CLong x) = toInteger x
instance Real CLong where
  toRational (CLong x) = toRational x

-- XXX only one of these is actually correct
newtype CFloat   = CFloat   FloatW
  deriving (Eq, Ord)
newtype CDouble  = CDouble  FloatW
  deriving (Eq, Ord)

-- Temporary conversion functions.
intToCSize :: Int -> CSize
intToCSize i = CSize (primIntToWord i)

cSizeToInt :: CSize -> Int
cSizeToInt (CSize i) = primWordToInt i
