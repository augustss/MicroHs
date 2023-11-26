module Foreign.Storable(Storable(..)) where
import Primitives
import Control.Error(undefined)
import Foreign.Ptr

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

foreign import ccall "wordPeek" c_wordPeek :: Ptr Word -> IO Word
foreign import ccall "wordPoke" c_wordPoke :: Ptr Word -> Word -> IO ()

instance Storable Word where
  sizeOf    _ = _wordSize
  alignment _ = _wordSize
  peek p      = c_wordPeek p
  poke p w    = c_wordPoke p w
