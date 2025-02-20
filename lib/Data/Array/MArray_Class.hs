module Data.Array.MArray_Class (
    MArray(..),
  ) where
import Data.Array.IxInternal
import Data.Ix

class (Monad m) => MArray a e m where
    getBounds       :: Ix i => a i e -> m (i,i)
    getNumElements  :: Ix i => a i e -> m Int
    newArray        :: Ix i => (i,i) -> e -> m (a i e)
    newArray_       :: Ix i => (i,i) -> m (a i e)
    unsafeNewArray_ :: Ix i => (i,i) -> m (a i e)

    unsafeRead      :: Ix i => a i e -> Int -> m e
    unsafeWrite     :: Ix i => a i e -> Int -> e -> m ()

    newArray (l,u) initialValue = do
        let n = safeRangeSize (l,u)
        marr <- unsafeNewArray_ (l,u)
        sequence_ [unsafeWrite marr i initialValue | i <- [0 .. n - 1]]
        return marr

    unsafeNewArray_ (l,u) = newArray (l,u) arrEleBottom

    newArray_ (l,u) = newArray (l,u) arrEleBottom

arrEleBottom :: a
arrEleBottom = error "MArray: undefined array element"
