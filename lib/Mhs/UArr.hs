module Mhs.UArr(
  UArr(..),
  newUArr,
  unsafeReadUArr,
  copyUArr,
  unsafeFreezeMutSTUArr,
  unsafeThawSTUArr,
  unsafeFreezeMutIOUArr,
  unsafeThawIOUArr,
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives(primPerformIO)
import Control.Monad.ST
import Control.Monad.ST_Type
import Data.ByteString(copy)
import Data.ByteString.Internal
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Mhs.MutUArr

newtype UArr a = UArr ByteString

newUArr :: forall s a . (Storable a) => Int -> [(Int, a)] -> UArr a
newUArr n ies = runST $ do
  a <- newMutSTUArr n
  mapM_ (uncurry (unsafeWriteMutSTUArr a)) ies
  unsafeFreezeMutSTUArr a

unsafeReadUArr :: (Storable a) => UArr a -> Int -> a
unsafeReadUArr (UArr bs) i = primPerformIO $
  withForeignPtr (primBS2FPtr bs) $ \ p ->
    peekElemOff (castPtr p) i

copyUArr :: UArr a -> UArr a
copyUArr (UArr a) = UArr (copy a)

unsafeFreezeMutSTUArr :: MutSTUArr s a -> ST s (UArr a)
unsafeFreezeMutSTUArr (MutSTUArr (MutIOUArr a)) = return (UArr a)

unsafeThawSTUArr :: UArr a -> ST s (MutSTUArr s a)
unsafeThawSTUArr (UArr a) = return (MutSTUArr (MutIOUArr a))

unsafeFreezeMutIOUArr :: MutIOUArr a -> IO (UArr a)
unsafeFreezeMutIOUArr (MutIOUArr a) = return (UArr a)

unsafeThawIOUArr :: UArr a -> IO (MutIOUArr a)
unsafeThawIOUArr (UArr a) = return (MutIOUArr a)
