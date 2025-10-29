module Mhs.Arr(
  Arr(..),
  newArr,
  sizeArr,
  unsafeReadArr,
  copyArr,
  unsafeFreezeMutSTArr,
  unsafeThawSTArr,
  unsafeFreezeMutIOArr,
  unsafeThawIOArr,
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives(primPerformIO, primArrSize)
import Control.Monad.ST
import Control.Monad.ST_Type
import Data.ByteString(copy)
import Data.ByteString.Internal
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Mhs.MutArr

newtype Arr a = Arr (MutIOArr a)

newArr :: forall a . Int -> [(Int, a)] -> Arr a
newArr n ies = primPerformIO $ do
  let undef = error "undefined array element"
  a <- newMutIOArr n undef
  mapM_ (uncurry (unsafeWriteMutIOArr a)) ies
  unsafeFreezeMutIOArr a

sizeArr :: Arr a -> Int
sizeArr (Arr a) = primPerformIO $ primArrSize a

unsafeReadArr :: Arr a -> Int -> a
unsafeReadArr (Arr a) i = primPerformIO $ unsafeReadMutIOArr a i

copyArr :: Arr a -> Arr a
copyArr (Arr a) = primPerformIO $ Arr <$> copyMutIOArr a

unsafeFreezeMutSTArr :: MutSTArr s a -> ST s (Arr a)
unsafeFreezeMutSTArr (MutSTArr a) = return (Arr a)

unsafeThawSTArr :: Arr a -> ST s (MutSTArr s a)
unsafeThawSTArr (Arr a) = return (MutSTArr a)

unsafeFreezeMutIOArr :: MutIOArr a -> IO (Arr a)
unsafeFreezeMutIOArr a = return (Arr a)

unsafeThawIOArr :: Arr a -> IO (MutIOArr a)
unsafeThawIOArr (Arr a) = return a
