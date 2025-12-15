module Data.ByteString.Internal(module Data.ByteString.Internal) where
import qualified Prelude()
import Primitives
import Control.DeepSeq.Class
import Control.Error
import Data.Bool
import Data.Coerce
import Data.Enum
import Data.Eq
import Data.Function
import Data.Int.Int
import Data.List (map)
import Data.List_Type
import Data.Monoid.Internal
import Data.Num
import Data.Ord
import Data.String
import Data.Word.Word8(Word8, intToWord8, word8ToInt)
import {-# SOURCE #-} Data.Typeable
import Foreign.C.Types(CChar)
import Text.Show

data ByteString  -- primitive type

type StrictByteString = ByteString

primBSappend  :: ByteString -> ByteString -> ByteString
primBSappend  = _primitive "bs++"
primBSEQ      :: ByteString -> ByteString -> Bool
primBSEQ      = _primitive "bs=="
primBSNE      :: ByteString -> ByteString -> Bool
primBSNE      = _primitive "bs/="
primBSLT      :: ByteString -> ByteString -> Bool
primBSLT      = _primitive "bs<"
primBSLE      :: ByteString -> ByteString -> Bool
primBSLE      = _primitive "bs<="
primBSGT      :: ByteString -> ByteString -> Bool
primBSGT      = _primitive "bs>"
primBSGE      :: ByteString -> ByteString -> Bool
primBSGE      = _primitive "bs>="
primBScmp     :: ByteString -> ByteString -> Ordering
primBScmp     = _primitive "bscmp"
primBSpack    :: [Word8] -> ByteString
primBSpack    = _primitive "bspack"
primBSunpack  :: ByteString -> [Word8]
primBSunpack  = _primitive "bsunpack"
primBSlength  :: ByteString -> Int
primBSlength  = _primitive "bslength"
primBSsubstr  :: ByteString -> Int -> Int -> ByteString
primBSsubstr  = _primitive "bssubstr"
primBSindex   :: ByteString -> Int -> Word8
primBSindex   = _primitive "bsindex"
primBSreplicate :: Int -> Word8 -> ByteString
primBSreplicate = _primitive "bsreplicate"

primPackCString :: Ptr CChar -> IO ByteString
primPackCString = _primitive "packCString"
primPackCStringLen :: Ptr CChar -> Int -> IO ByteString
primPackCStringLen = _primitive "packCStringLen"

primBS2FPtr :: ByteString -> ForeignPtr CChar
primBS2FPtr = _primitive "bs2fp"

-- Warning: This function modifies the `ForeignPtr`,
-- avoid using the `ForeignPtr` after calling `primFPtr2BS`.
primFPtr2BS :: ForeignPtr CChar -> Int -> ByteString
primFPtr2BS = _primitive "fp2bs"

primBSgrab :: Ptr CChar -> IO ByteString
primBSgrab = _primitive "bsgrab"

-----------------------------------------

instance NFData ByteString

instance Eq ByteString where
  (==) = primBSEQ
  (/=) = primBSNE

instance Ord ByteString where
  compare = primBScmp
  (<)     = primBSLT
  (<=)    = primBSLE
  (>)     = primBSGT
  (>=)    = primBSGE

instance Show ByteString where
  showsPrec p bs = showsPrec p (toString bs)

instance IsString ByteString where
  fromString = coerce pack

instance Semigroup ByteString where
  (<>) = append

instance Monoid ByteString where
  mempty = empty

toString :: ByteString -> String
toString = coerce unpack

empty :: ByteString
empty = pack []

singleton :: Word8 -> ByteString
singleton c = pack [c]

length :: ByteString -> Int
length = primBSlength

append :: ByteString -> ByteString -> ByteString
append = primBSappend

substr :: ByteString -> Int -> Int -> ByteString
substr bs offs len
  | offs < 0 || offs > sz     = bsError "substr: bad offset"
  | len < 0  || len > sz-offs = bsError "substr: bad length"
  | otherwise = primBSsubstr bs offs len
  where sz = length bs

bsError :: String -> a
bsError s = error $ "Data.ByteString." ++ s

pack :: [Word8] -> ByteString
pack = primBSpack

unpack :: ByteString -> [Word8]
unpack = primBSunpack

null :: ByteString -> Bool
null bs = length bs == 0

-- Take a C string and turn it into a ByteString.
-- This will take ownership of the memory which will eventually
-- be free()d.  The pointer should not be used by the caller after this call.
grabCString :: Ptr CChar -> IO ByteString
grabCString = primBSgrab

-- Are the bytestrings the same underlying object?
sameByteString :: ByteString -> ByteString -> Bool
sameByteString bs1 bs2 =
  let r = primPtrToWord (primForeignPtrToPtr (primBS2FPtr bs1)) == primPtrToWord (primForeignPtrToPtr (primBS2FPtr bs2))
  in  r `seq` bs1 `primSeq` bs2 `primSeq` r

c2w :: Char -> Word8
c2w = intToWord8 . primOrd

w2c :: Word8 -> Char
w2c = primChr . word8ToInt

toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int)
toForeignPtr bs = (primUnsafeCoerce (primBS2FPtr bs), 0, primBSlength bs)
