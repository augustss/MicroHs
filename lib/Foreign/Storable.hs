-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Foreign.Storable(Storable(..)) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error(undefined)
import Foreign.C.Types
import Foreign.Ptr
import Data.Bool_Type
import Data.Int
import Data.Word
import Data.Coerce
import {-# SOURCE #-} Data.Typeable

class Storable a where
   sizeOf      :: a -> Int
   alignment   :: a -> Int
   peekElemOff :: Ptr a -> Int      -> IO a
   pokeElemOff :: Ptr a -> Int -> a -> IO ()
   peekByteOff :: forall b . Ptr b -> Int      -> IO a
   pokeByteOff :: forall b . Ptr b -> Int -> a -> IO ()
   peek        :: Ptr a      -> IO a
   poke        :: Ptr a -> a -> IO ()

   peekElemOff ptr off     = peekByteOff ptr (off `primIntMul` sizeOf (undefined :: a))
   pokeElemOff ptr off val = pokeByteOff ptr (off `primIntMul` sizeOf val) val

   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)

   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0

foreign import ccall "peekWord" c_peekWord :: Ptr Word -> IO Word
foreign import ccall "pokeWord" c_pokeWord :: Ptr Word -> Word -> IO ()

wordSizeInBytes :: Int
wordSizeInBytes = _wordSize `primIntQuot` 8

instance Storable Word where
  sizeOf    _ = wordSizeInBytes
  alignment _ = wordSizeInBytes
  peek p      = c_peekWord p
  poke p w    = c_pokeWord p w

instance Storable Int where
  sizeOf    _ = wordSizeInBytes
  alignment _ = wordSizeInBytes
  peek p      = c_peekWord (castPtr p) `primBind` \ w -> primReturn (primWordToInt w)
  poke p w    = c_pokeWord (castPtr p) (primIntToWord w)

instance Storable Char where
  sizeOf    _ = 4
  alignment _ = 4
  peek p      = c_peek_int32 (castPtr p) `primBind` \ i -> primReturn (primChr (primUnsafeCoerce i))
  poke p c    = c_poke_int32 (castPtr p) (primUnsafeCoerce (primOrd c))

instance Storable Bool where
  sizeOf    _ = intSize
  alignment _ = intSize
  peek p      = c_peek_int (castPtr p) `primBind` \(CInt i) -> primReturn (i `primIntNE` 0)
  poke p b    = c_poke_int (castPtr p) (if b then CInt 1 else CInt 0)

foreign import ccall "peekPtr" c_peekPtr :: Ptr (Ptr ()) -> IO (Ptr ())
foreign import ccall "pokePtr" c_pokePtr :: Ptr (Ptr ()) -> Ptr () -> IO ()

instance Storable (Ptr a) where
  sizeOf    _ = wordSizeInBytes
  alignment _ = wordSizeInBytes
  peek p      = c_peekPtr (castPtr p) `primBind` \ q -> primReturn (castPtr q)
  poke p w    = c_pokePtr (castPtr p) (castPtr w)

instance Storable (FunPtr a) where
  sizeOf    _ = wordSizeInBytes
  alignment _ = wordSizeInBytes
  peek p      = c_peekPtr (castPtr p) `primBind` \ q -> primReturn (castPtrToFunPtr (castPtr q))
  poke p w    = c_pokePtr (castPtr p) (castPtr (castFunPtrToPtr w))

foreign import ccall "peek_uint8" c_peek_uint8 :: Ptr Word8 -> IO Word8
foreign import ccall "poke_uint8" c_poke_uint8 :: Ptr Word8 -> Word8 -> IO ()

instance Storable Word8 where
  sizeOf    _ = 1
  alignment _ = 1
  peek p      = c_peek_uint8 p
  poke p w    = c_poke_uint8 p w

foreign import ccall "peek_uint16" c_peek_uint16 :: Ptr Word16 -> IO Word16
foreign import ccall "poke_uint16" c_poke_uint16 :: Ptr Word16 -> Word16 -> IO ()

instance Storable Word16 where
  sizeOf    _ = 2
  alignment _ = 2
  peek p      = c_peek_uint16 p
  poke p w    = c_poke_uint16 p w

foreign import ccall "peek_uint32" c_peek_uint32 :: Ptr Word32 -> IO Word32
foreign import ccall "poke_uint32" c_poke_uint32 :: Ptr Word32 -> Word32 -> IO ()

instance Storable Word32 where
  sizeOf    _ = 4
  alignment _ = 4
  peek p      = c_peek_uint32 p
  poke p w    = c_poke_uint32 p w

foreign import ccall "peek_uint64" c_peek_uint64 :: Ptr Word64 -> IO Word64
foreign import ccall "poke_uint64" c_poke_uint64 :: Ptr Word64 -> Word64 -> IO ()

instance Storable Word64 where
  sizeOf    _ = 8
  alignment _ = 8
  peek p      = c_peek_uint64 p
  poke p w    = c_poke_uint64 p w

foreign import ccall "peek_int8" c_peek_int8 :: Ptr Int8 -> IO Int8
foreign import ccall "poke_int8" c_poke_int8 :: Ptr Int8 -> Int8 -> IO ()

instance Storable Int8 where
  sizeOf    _ = 1
  alignment _ = 1
  peek p      = c_peek_int8 p
  poke p w    = c_poke_int8 p w

foreign import ccall "peek_int16" c_peek_int16 :: Ptr Int16 -> IO Int16
foreign import ccall "poke_int16" c_poke_int16 :: Ptr Int16 -> Int16 -> IO ()

instance Storable Int16 where
  sizeOf    _ = 2
  alignment _ = 2
  peek p      = c_peek_int16 p
  poke p w    = c_poke_int16 p w

foreign import ccall "peek_int32" c_peek_int32 :: Ptr Int32 -> IO Int32
foreign import ccall "poke_int32" c_poke_int32 :: Ptr Int32 -> Int32 -> IO ()

instance Storable Int32 where
  sizeOf    _ = 4
  alignment _ = 4
  peek p      = c_peek_int32 p
  poke p w    = c_poke_int32 p w

foreign import ccall "peek_int64" c_peek_int64 :: Ptr Int64 -> IO Int64
foreign import ccall "poke_int64" c_poke_int64 :: Ptr Int64 -> Int64 -> IO ()

instance Storable Int64 where
  sizeOf    _ = 8
  alignment _ = 8
  peek p      = c_peek_int64 p
  poke p w    = c_poke_int64 p w

foreign import ccall "peek_char" c_peek_char :: Ptr CChar -> IO CChar
foreign import ccall "poke_char" c_poke_char :: Ptr CChar -> CChar -> IO ()

instance Storable CChar where
  sizeOf    _ = charSize
  alignment _ = charSize
  peek        = c_peek_char
  poke        = c_poke_char

foreign import ccall "peek_schar" c_peek_schar :: Ptr CSChar -> IO CSChar
foreign import ccall "poke_schar" c_poke_schar :: Ptr CSChar -> CSChar -> IO ()

instance Storable CSChar where
  sizeOf    _ = charSize
  alignment _ = charSize
  peek        = c_peek_schar
  poke        = c_poke_schar

foreign import ccall "peek_uchar" c_peek_uchar :: Ptr CUChar -> IO CUChar
foreign import ccall "poke_uchar" c_poke_uchar :: Ptr CUChar -> CUChar -> IO ()

instance Storable CUChar where
  sizeOf    _ = charSize
  alignment _ = charSize
  peek        = c_peek_uchar
  poke        = c_poke_uchar

foreign import ccall "peek_short" c_peek_short :: Ptr CShort -> IO CShort
foreign import ccall "poke_short" c_poke_short :: Ptr CShort -> CShort -> IO ()

instance Storable CShort where
  sizeOf    _ = shortSize
  alignment _ = shortSize
  peek        = c_peek_short
  poke        = c_poke_short

foreign import ccall "peek_ushort" c_peek_ushort :: Ptr CUShort -> IO CUShort
foreign import ccall "poke_ushort" c_poke_ushort :: Ptr CUShort -> CUShort -> IO ()

instance Storable CUShort where
  sizeOf    _ = shortSize
  alignment _ = shortSize
  peek        = c_peek_ushort
  poke        = c_poke_ushort

foreign import ccall "peek_int" c_peek_int :: Ptr CInt -> IO CInt
foreign import ccall "poke_int" c_poke_int :: Ptr CInt -> CInt -> IO ()

instance Storable CInt where
  sizeOf    _ = intSize
  alignment _ = intSize
  peek p      = c_peek_int p
  poke p w    = c_poke_int p w

foreign import ccall "peek_uint" c_peek_uint :: Ptr CUInt -> IO CUInt
foreign import ccall "poke_uint" c_poke_uint :: Ptr CUInt -> CUInt -> IO ()

instance Storable CUInt where
  sizeOf    _ = intSize
  alignment _ = intSize
  peek p      = c_peek_uint p
  poke p w    = c_poke_uint p w

foreign import ccall "peek_long" c_peek_long :: Ptr CLong -> IO CLong
foreign import ccall "poke_long" c_poke_long :: Ptr CLong -> CLong -> IO ()

instance Storable CLong where
  sizeOf    _ = longSize
  alignment _ = longSize
  peek p      = c_peek_long p
  poke p w    = c_poke_long p w

foreign import ccall "peek_ulong" c_peek_ulong :: Ptr CULong -> IO CULong
foreign import ccall "poke_ulong" c_poke_ulong :: Ptr CULong -> CULong -> IO ()

instance Storable CULong where
  sizeOf    _ = longSize
  alignment _ = longSize
  peek p      = c_peek_ulong p
  poke p w    = c_poke_ulong p w

foreign import ccall "peek_llong" c_peek_llong :: Ptr CLLong -> IO CLLong
foreign import ccall "poke_llong" c_poke_llong :: Ptr CLLong -> CLLong -> IO ()

instance Storable CLLong where
  sizeOf    _ = llongSize
  alignment _ = llongSize
  peek p      = c_peek_llong p
  poke p w    = c_poke_llong p w

foreign import ccall "peek_ullong" c_peek_ullong :: Ptr CULLong -> IO CULLong
foreign import ccall "poke_ullong" c_poke_ullong :: Ptr CULLong -> CULLong -> IO ()

instance Storable CULLong where
  sizeOf    _ = llongSize
  alignment _ = llongSize
  peek p      = c_peek_ullong p
  poke p w    = c_poke_ullong p w

foreign import ccall "peek_size_t" c_peek_size_t :: Ptr CSize -> IO CSize
foreign import ccall "poke_size_t" c_poke_size_t :: Ptr CSize -> CSize -> IO ()

instance Storable CSize where
  sizeOf    _ = sizeTSize
  alignment _ = sizeTSize
  peek p      = c_peek_size_t p
  poke p w    = c_poke_size_t p w

foreign import ccall "peek_long" c_peek_time_t :: Ptr CTime -> IO CTime
foreign import ccall "poke_long" c_poke_time_t :: Ptr CTime -> CTime -> IO ()

instance Storable CTime where
  sizeOf    _ = longSize
  alignment _ = longSize
  peek p      = c_peek_time_t p
  poke p w    = c_poke_time_t p w

foreign import ccall "peek_flt32" c_peek_flt32 :: Ptr Float -> IO Float
foreign import ccall "poke_flt32" c_poke_flt32 :: Ptr Float -> Float -> IO ()

instance Storable Float where
  sizeOf    _ = 4
  alignment _ = 4
  peek p      = c_peek_flt32 p
  poke p w    = c_poke_flt32 p w

instance Storable CFloat where
  sizeOf    _ = 4
  alignment _ = 4
  peek p      = c_peek_flt32 (castPtr p) `primBind` \f -> primReturn (CFloat f)
  poke p (CFloat w) = c_poke_flt32 (castPtr p) w

foreign import ccall "peek_flt64" c_peek_flt64 :: Ptr Double -> IO Double
foreign import ccall "poke_flt64" c_poke_flt64 :: Ptr Double -> Double -> IO ()

instance Storable Double where
  sizeOf    _ = 8
  alignment _ = 8
  peek p      = c_peek_flt64 p
  poke p w    = c_poke_flt64 p w

instance Storable CDouble where
  sizeOf    _ = 8
  alignment _ = 8
  peek p      = c_peek_flt64 (castPtr p) `primBind` \d -> primReturn (CDouble d)
  poke p (CDouble w) = c_poke_flt64 (castPtr p) w

foreign import ccall "sizeof_char" c_sizeof_char :: IO Int
foreign import ccall "sizeof_short" c_sizeof_short :: IO Int
foreign import ccall "sizeof_int" c_sizeof_int :: IO Int
foreign import ccall "sizeof_long" c_sizeof_long :: IO Int
foreign import ccall "sizeof_llong" c_sizeof_llong :: IO Int
foreign import ccall "sizeof_size_t" c_sizeof_size_t :: IO Int
charSize :: Int
charSize = primPerformIO c_sizeof_char
shortSize :: Int
shortSize = primPerformIO c_sizeof_short
intSize :: Int
intSize = primPerformIO c_sizeof_int
longSize :: Int
longSize = primPerformIO c_sizeof_long
llongSize :: Int
llongSize = primPerformIO c_sizeof_llong
sizeTSize :: Int
sizeTSize = primPerformIO c_sizeof_size_t

deriving newtype instance Storable IntPtr
deriving newtype instance Storable WordPtr
deriving newtype instance Storable CBool
deriving newtype instance Storable CClock
