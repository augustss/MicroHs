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
 intToCSize, cSizeToInt,
 ) where
import Prelude()
import Primitives
import Data.Word

-- The MicroHs Word type is the "natural" architecture word size;
-- it is the same as the pointer difference type.
-- And Int is the natural signed word size.
newtype CChar    = CChar    Char
newtype CSChar   = CSChar   Int
newtype CUChar   = CUChar   Word
newtype CShort   = CShort   Int
newtype CUShort  = CUShort  Word
newtype CInt     = CInt     Int
newtype CUInt    = CUInt    Word
newtype CLong    = CLong    Int
newtype CULong   = CULong   Word
newtype CPtrdiff = CPtrdiff Word
newtype CSize    = CSize    Word
newtype CSSize   = CSSize   Int
newtype CLLong   = CLLong   Int
newtype CULLong  = CULLong  Word
newtype CIntPtr  = CIntPtr  Int
newtype CUIntPtr = CUIntPtr Word

newtype CFloat   = CFloat   FloatW
newtype CDouble  = CDouble  FloatW

-- Temporary conversion functions.
intToCSize :: Int -> CSize
intToCSize i = CSize (primIntToWord i)

cSizeToInt :: CSize -> Int
cSizeToInt (CSize i) = primWordToInt i
