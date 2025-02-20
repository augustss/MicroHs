module Data.Array.IArray_Class(module Data.Array.IArray_Class) where
import Control.Monad.ST(ST, runST)
import Data.Array.IxInternal
import Data.Array.ST_Type(STArray)
import Data.Array.MArray_Class
import Data.Ix(Ix)

class IArray a e where
  bounds           :: Ix i => a i e -> (i,i)
  numElements      :: Ix i => a i e -> Int
  unsafeArray      :: Ix i => (i,i) -> [(Int, e)] -> a i e
  unsafeAt         :: Ix i => a i e -> Int -> e
  unsafeReplace    :: Ix i => a i e -> [(Int, e)] -> a i e
  unsafeAccum      :: Ix i => (e -> e' -> e) -> a i e -> [(Int, e')] -> a i e
  unsafeAccumArray :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> a i e

  unsafeReplace arr ies = runST (unsafeReplaceST arr ies >>= unsafeFreeze)
  unsafeAccum f arr ies = runST (unsafeAccumST f arr ies >>= unsafeFreeze)
  unsafeAccumArray f e lu ies = runST (unsafeAccumArrayST f e lu ies >>= unsafeFreeze)

unsafeReplaceST :: (IArray a e, Ix i) => a i e -> [(Int, e)] -> ST s (STArray s i e)
unsafeReplaceST arr ies = do
    marr <- thaw arr
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    return marr

unsafeAccumST :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumST f arr ies = do
    marr <- thaw arr
    sequence_ [do old <- unsafeRead marr i
                  unsafeWrite marr i (f old new)
              | (i, new) <- ies]
    return marr

unsafeAccumArrayST :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumArrayST f e (l,u) ies = do
    marr <- newArray (l,u) e
    sequence_ [do old <- unsafeRead marr i
                  unsafeWrite marr i (f old new)
              | (i, new) <- ies]
    return marr

unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = freeze

listArray :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
listArray (l,u) es =
    let n = safeRangeSize (l,u)
    in unsafeArray (l,u) (zip [0 .. n - 1] es)

freeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
freeze marr = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  es <- mapM (unsafeRead marr) [0 .. n - 1]
  return (listArray (l,u) es)

thaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
thaw arr = case bounds arr of
  (l,u) -> do
    marr <- newArray_ (l,u)
    let n = safeRangeSize (l,u)
    sequence_ [ unsafeWrite marr i (unsafeAt arr i)
              | i <- [0 .. n - 1]]
    return marr

---XXXXX
instance MArray (STArray s) a (ST s)
