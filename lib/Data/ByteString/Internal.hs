module Data.ByteString.Internal(module Data.ByteString.Internal) where
import Prelude(); import MiniPrelude hiding(length)
import Control.DeepSeq.Class
import Data.Word(Word8)

data ByteString  -- primitive type

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
  fromString = pack . map (toEnum . fromEnum)

instance Semigroup ByteString where
  (<>) = append

instance Monoid ByteString where
  mempty = empty

toString :: ByteString -> String
toString = map (toEnum . fromEnum) . unpack

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
