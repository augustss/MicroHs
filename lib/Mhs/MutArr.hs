-- Temporary module
module Mhs.MutArr(module Mhs.MutArr, MutIOArr) where
import qualified Prelude(); import MiniPrelude              -- do not import Prelude
import Primitives(Int, IO, IOArray, primArrAlloc, primArrSize, primArrRead, primArrWrite, primArrCopy, primArrTrunc)
import Control.Monad.ST_Type
import Mhs.Builtin

-- primitive IOArray
type MutIOArr a = IOArray a

newMutIOArr :: forall a . Int -> a -> IO (MutIOArr a)
newMutIOArr = primArrAlloc

sizeMutIOArr :: forall a . MutIOArr a -> IO Int
sizeMutIOArr = primArrSize

unsafeReadMutIOArr :: forall a . MutIOArr a -> Int -> IO a
unsafeReadMutIOArr = primArrRead

unsafeWriteMutIOArr :: forall a . MutIOArr a -> Int -> a -> IO ()
unsafeWriteMutIOArr = primArrWrite

copyMutIOArr :: forall a . MutIOArr a -> IO (MutIOArr a)
copyMutIOArr = primArrCopy

shrinkMutIOArr :: forall a . MutIOArr a -> Int -> IO ()
shrinkMutIOArr = primArrTrunc

----------------

newtype MutSTArr s a = MutSTArr (MutIOArr a)

newMutSTArr :: forall s a . Int -> a -> ST s (MutSTArr s a)
newMutSTArr n a = ST $ MutSTArr <$> newMutIOArr n a

sizeMutSTArr :: forall s a . MutSTArr s a -> ST s Int
sizeMutSTArr (MutSTArr a) = ST $ sizeMutIOArr a

unsafeReadMutSTArr :: forall s a . MutSTArr s a -> Int -> ST s a
unsafeReadMutSTArr (MutSTArr a) i = ST $ unsafeReadMutIOArr a i

unsafeWriteMutSTArr :: forall s a . MutSTArr s a -> Int -> a -> ST s ()
unsafeWriteMutSTArr (MutSTArr a) i x = ST $ unsafeWriteMutIOArr a i x

copyMutSTArr :: forall s a . MutSTArr s a -> ST s (MutSTArr s a)
copyMutSTArr (MutSTArr a) = ST $ MutSTArr <$> copyMutIOArr a

shrinkMutSTArr :: forall s a . MutSTArr s a -> Int -> ST s ()
shrinkMutSTArr (MutSTArr a) n = ST $ shrinkMutIOArr a n
