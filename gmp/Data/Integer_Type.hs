module Data.Integer_Type(
  Integer(..),
  MPZ, newMPZ,
  _intToInteger, _wordToInteger, _integerToInt, _integerToWord,
  _integerToFloatW,
  ) where
import Prelude()
import Primitives
--import Foreign.ForeignPtr
--import Mhs.Builtin
--import System.IO.Unsafe

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fp io = 
  io (primForeignPtrToPtr fp) `primBind` \ b ->
  primSeq fp (primReturn b)

data MPZ

newtype Integer = I (ForeignPtr MPZ)

foreign import capi "gmp.h new_mpz" new_mpz :: IO (Ptr MPZ)  -- it really returns a ForeignPtr

newMPZ :: IO (ForeignPtr MPZ)
newMPZ = 
  new_mpz `primBind` \ x ->
  primReturn (primUnsafeCoerce x)

foreign import capi "gmp.h mpz_init_set_si" mpz_init_set_si :: Ptr MPZ -> Int -> IO ()
_intToInteger :: Int -> Integer
_intToInteger i = primPerformIO (
  newMPZ `primBind` \ x -> 
  withForeignPtr x ( \ p -> mpz_init_set_si p i) `primThen`
  primReturn (I x)
  )

foreign import capi "gmp.h mpz_init_set_ui" mpz_init_set_ui :: Ptr MPZ -> Word -> IO ()
_wordToInteger :: Word -> Integer
_wordToInteger i = primPerformIO (do
  newMPZ `primBind` \ x ->
  withForeignPtr x ( \ p -> mpz_init_set_ui p i) `primThen`
  primReturn (I x)
  )

foreign import capi "gmp.h mpz_get_si" mpz_get_si :: Ptr MPZ -> IO Int
_integerToInt :: Integer -> Int
_integerToInt (I x) = primPerformIO (withForeignPtr x mpz_get_si)

foreign import capi "gmp.h mpz_get_ui" mpz_get_ui :: Ptr MPZ -> IO Word
_integerToWord :: Integer -> Word
_integerToWord (I x) = primPerformIO (withForeignPtr x mpz_get_ui)

foreign import capi "gmp.h mpz_get_d" mpz_get_d :: Ptr MPZ -> IO FloatW
_integerToFloatW :: Integer -> FloatW
_integerToFloatW (I x) = primPerformIO (withForeignPtr x mpz_get_d)
