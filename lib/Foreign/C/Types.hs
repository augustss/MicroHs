-- Types used for C FFI.
module Foreign.C.Types(
  CChar(..),  CSChar(..),  CUChar(..), CWchar(..),
  CShort(..), CUShort(..),
  CInt(..),   CUInt(..),
  CLong(..),  CULong(..),
  CPtrdiff(..),
  CSize(..),  CSSize(..),
  CLLong(..), CULLong(..),
  CIntMax(..), CUIntMax(..),
  CIntPtr(..), CUIntPtr(..),
  CFloat(..), CDouble(..),
  CSigAtomic(..),
  CTime(..),
  intToCSize, cSizeToInt,
  ) where
import qualified Prelude()
import Primitives
import Data.Bool
import Data.Bounded
import Data.Coerce
import Data.Enum
import Data.Eq
import Data.Floating
import Data.Fractional
import Data.Int.Int
import Data.Integral
import Data.Num
import Data.Ord
import Data.Real
import Data.RealFrac
import Data.RealFloat
import Data.Word.Word
import Data.Float
import Data.Double
import {-# SOURCE #-} Data.Typeable

-- The MicroHs Word type is the "natural" architecture word size;
-- it is the same as the pointer difference type.
-- And Int is the natural signed word size.
newtype CChar    = CChar    Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CSChar   = CSChar   Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CUChar   = CUChar   Word
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CWchar = CWchar Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CShort   = CShort   Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CUShort  = CUShort  Word
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CInt     = CInt     Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CUInt    = CUInt    Word
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CLong    = CLong    Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CULong   = CULong   Word
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CPtrdiff = CPtrdiff Word
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CSize    = CSize    Word
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CSSize   = CSSize   Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CLLong   = CLLong   Int64
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CULLong  = CULLong  Word64
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CIntPtr  = CIntPtr  Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CUIntPtr = CUIntPtr Word
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CIntMax = CIntMax Int64
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CUIntMax = CUIntMax Word64
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)
newtype CSigAtomic = CSigAtomic Word  -- XXX platform specific
  deriving (Eq, Ord, Enum, Num, Integral, Real, Bounded)

-- XXX This is really platform specific
newtype CTime = CTime Int
  deriving (Eq, Ord)

-- XXX only one of these is actually correct
newtype CFloat   = CFloat   Float
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)
newtype CDouble  = CDouble  Double
  deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)

-- Temporary conversion functions.
intToCSize :: Int -> CSize
intToCSize i = CSize (primIntToWord i)

cSizeToInt :: CSize -> Int
cSizeToInt (CSize i) = primWordToInt i
