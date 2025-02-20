module Data.Array.MArray (
    MArray(newArray, newArray_, getBounds),
    module Data.Ix,
    newListArray,
    newGenArray,
    readArray,
    writeArray,
    modifyArray,
    modifyArray',
    foldlMArray',
    foldrMArray',
    mapMArrayM_,
    forMArrayM_,
    foldlMArrayM',
    foldrMArrayM',
    mapArray,
    mapIndices,
    getElems,
    getAssocs,
    freeze,
    thaw,
  ) where
import Data.Array.IArray(IArray)
import Data.Array.IxInternal
import Data.Array.MArray_Class
import Data.Ix

newListArray :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)
newListArray (l,u) es = do
    marr <- newArray_ (l,u)
    let n = safeRangeSize (l,u)
        f x k i
            | i == n    = return ()
            | otherwise = unsafeWrite marr i x >> k (i+1)
    foldr f (\ !_i -> return ()) es 0
    return marr

newGenArray :: (MArray a e m, Ix i) => (i,i) -> (i -> m e) -> m (a i e)
newGenArray bnds f = do
    let n = safeRangeSize bnds
    marr <- unsafeNewArray_ bnds
    let g ix k i
            | i == n    = return ()
            | otherwise = do
                x <- f ix
                unsafeWrite marr i x
                k (i+1)
    foldr g (\ !_i -> return ()) (range bnds) 0
    return marr

readArray marr i = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  unsafeRead marr (safeIndex (l,u) n i)

writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
writeArray marr i e = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  unsafeWrite marr (safeIndex (l,u) n i) e

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray marr i f = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  let idx = safeIndex (l,u) n i
  x <- unsafeRead marr idx
  unsafeWrite marr idx (f x)

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' marr i f = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  let idx = safeIndex (l,u) n i
  x <- unsafeRead marr idx
  let !x' = f x
  unsafeWrite marr idx x'

mapArray :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
mapArray f marr = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  marr' <- newArray_ (l,u)
  sequence_ [do e <- unsafeRead marr i
                unsafeWrite marr' i (f e)
            | i <- [0 .. n - 1]]
  return marr'

mapIndices :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)
mapIndices (l',u') f marr = do
    marr' <- newArray_ (l',u')
    n' <- getNumElements marr'
    sequence_ [do e <- readArray marr (f i')
                  unsafeWrite marr' (safeIndex (l',u') n' i') e
              | i' <- range (l',u')]
    return marr'

getElems :: (MArray a e m, Ix i) => a i e -> m [e]
getElems marr = do
  (_l, _u) <- getBounds marr
  n <- getNumElements marr
  sequence [unsafeRead marr i | i <- [0 .. n - 1]]

getAssocs :: (MArray a e m, Ix i) => a i e -> m [(i, e)]
getAssocs marr = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  sequence [ do e <- unsafeRead marr (safeIndex (l,u) n i); return (i,e)
           | i <- range (l,u)]

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

foldlMArray' :: (MArray a e m, Ix i) => (b -> e -> b) -> b -> a i e -> m b
foldlMArray' f = foldlMArrayM' (\z x -> pure (f z x))

foldrMArray' :: (MArray a e m, Ix i) => (e -> b -> b) -> b -> a i e -> m b
foldrMArray' f = foldrMArrayM' (\x z -> pure (f x z))

foldlMArrayM' :: (MArray a e m, Ix i) => (b -> e -> m b) -> b -> a i e -> m b
foldlMArrayM' f z0 = \a -> do
    n <- getNumElements a
    let go z i | i >= n = pure z
               | otherwise = do
                   x <- unsafeRead a i
                   z' <- f z x
                   go z' (i+1)
    go z0 0

foldrMArrayM' :: (MArray a e m, Ix i) => (e -> b -> m b) -> b -> a i e -> m b
foldrMArrayM' f z0 = \a -> do
    n <- getNumElements a
    let go i z | i < 0 = pure z
               | otherwise = do
                   x <- unsafeRead a i
                   z' <- f x z
                   go (i-1) z'
    go (n-1) z0

mapMArrayM_ :: (MArray a e m, Ix i) => (e -> m b) -> a i e -> m ()
mapMArrayM_ f = \a -> do
    n <- getNumElements a
    let go i | i >= n = pure ()
             | otherwise = do
                 x <- unsafeRead a i
                 _ <- f x
                 go (i+1)
    go 0

forMArrayM_ :: (MArray a e m, Ix i) => a i e -> (e -> m b) -> m ()
forMArrayM_ = flip mapMArrayM_
