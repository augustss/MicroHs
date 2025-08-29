module Data.Integer_Type(
  Integer(..),
  MPZ, newMPZ,
  _intToInteger, _wordToInteger, _int64ToInteger, _word64ToInteger,
  _integerToInt, _integerToWord, _integerToInt64, _integerToWord64,
  _integerToFloat,
  _integerToDouble,
  ) where
import qualified Prelude()
import Primitives
import Control.Error
import Data.Function(($))
import {-# SOURCE #-} Data.Typeable
--import Foreign.ForeignPtr
--import Mhs.Builtin
--import System.IO.Unsafe

-- We cannot import Foreign.ForeignPtr; it is circular.
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fp io =
  io (primForeignPtrToPtr fp) `primBind` \ b ->
  primSeq fp (primReturn b)

-----

data MPZ

newtype Integer = I (ForeignPtr MPZ)

foreign import capi "new_mpz"         new_mpz         :: IO (Ptr MPZ)  -- it really returns a ForeignPtr
foreign import capi "mpz_init_set_si" mpz_init_set_si :: Ptr MPZ -> Int -> IO ()
foreign import capi "mpz_init_set_ui" mpz_init_set_ui :: Ptr MPZ -> Word -> IO ()
foreign import capi "mpz_get_si"      mpz_get_si      :: Ptr MPZ -> IO Int
foreign import capi "mpz_get_f"       mpz_get_f       :: Ptr MPZ -> IO Float
foreign import capi "mpz_get_d"       mpz_get_d       :: Ptr MPZ -> IO Double

newMPZ :: IO (ForeignPtr MPZ)
newMPZ =
  new_mpz `primBind` \ x ->
  primReturn (primUnsafeCoerce x)

_intToInteger :: Int -> Integer
_intToInteger i = primPerformIO (
  newMPZ `primBind` \ x ->
  withForeignPtr x ( \ p -> mpz_init_set_si p i) `primThen`
  primReturn (I x)
  )

_wordToInteger :: Word -> Integer
_wordToInteger i = primPerformIO (do
  newMPZ `primBind` \ x ->
  withForeignPtr x ( \ p -> mpz_init_set_ui p i) `primThen`
  primReturn (I x)
  )

_integerToInt :: Integer -> Int
_integerToInt (I x) = primPerformIO (withForeignPtr x mpz_get_si)

_integerToWord :: Integer -> Word
_integerToWord i = primIntToWord (_integerToInt i)

_integerToFloat :: Integer -> Float
_integerToFloat (I x) = primPerformIO (withForeignPtr x mpz_get_f)

_integerToDouble :: Integer -> Double
_integerToDouble (I x) = primPerformIO (withForeignPtr x mpz_get_d)

-------

foreign import capi "mpz_init_set_si64" mpz_init_set_si64 :: Ptr MPZ -> Int64 -> IO ()
foreign import capi "mpz_init_set_ui64" mpz_init_set_ui64 :: Ptr MPZ -> Word64 -> IO ()
foreign import capi "mpz_get_si64"      mpz_get_si64      :: Ptr MPZ -> IO Int64

_integerToInt64 :: Integer -> Int64
_integerToInt64 (I x) = primPerformIO (withForeignPtr x mpz_get_si64)

_integerToWord64 :: Integer -> Word64
_integerToWord64 i = primInt64ToWord64 (_integerToInt64 i)

_word64ToInteger :: Word64 -> Integer
_word64ToInteger i = primPerformIO (do
  newMPZ `primBind` \ x ->
  withForeignPtr x ( \ p -> mpz_init_set_ui64 p i) `primThen`
  primReturn (I x)
  )

_int64ToInteger :: Int64 -> Integer
_int64ToInteger i = primPerformIO (
  newMPZ `primBind` \ x ->
  withForeignPtr x ( \ p -> mpz_init_set_si64 p i) `primThen`
  primReturn (I x)
  )
