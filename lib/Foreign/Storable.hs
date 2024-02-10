module Foreign.Storable(Storable(..)) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error(undefined)
import Foreign.Ptr
import Data.Word8

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

foreign import ccall "peekPtr" c_peekPtr :: forall a . Ptr (Ptr a) -> IO (Ptr a)
foreign import ccall "pokePtr" c_pokePtr :: forall a . Ptr (Ptr a) -> Ptr a -> IO ()

instance forall a . Storable (Ptr a) where
  sizeOf    _ = wordSizeInBytes
  alignment _ = wordSizeInBytes
  peek p      = c_peekPtr p
  poke p w    = c_pokePtr p w

foreign import ccall "peekByte" c_peekByte :: Ptr Word8 -> IO Word8
foreign import ccall "pokeByte" c_pokeByte :: Ptr Word8 -> Word8 -> IO ()

instance Storable Word8 where
  sizeOf    _ = 1
  alignment _ = 1
  peek p      = c_peekByte p
  poke p w    = c_pokeByte p w
