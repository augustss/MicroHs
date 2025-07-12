module Foreign.Marshal.Array (
  mallocArray, mallocArray0,
  allocaArray, allocaArray0,
  reallocArray, reallocArray0,
  callocArray, callocArray0,
  peekArray, peekArray0,
  pokeArray, pokeArray0,
  newArray, newArray0,
  withArray, withArray0,
  withArrayLen, withArrayLen0,
  copyArray, moveArray,
  lengthArray0,
  advancePtr,
) where

import qualified Prelude(); import MiniPrelude
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (copyBytes, moveBytes)

mallocArray :: forall a . Storable a => Int -> IO (Ptr a)
mallocArray size = mallocBytes (size * sizeOf (error "mallocArray" :: a))

mallocArray0 :: forall a . Storable a => Int -> IO (Ptr a)
mallocArray0 size  = mallocArray (size + 1)

allocaArray :: forall a b. (Storable a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray n = allocaBytes (n * sizeOf (error "allocaArray" :: a))

allocaArray0 :: forall a b. (Storable a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 n = allocaArray (n + 1)

reallocArray :: forall a . Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray p n = reallocBytes p (n * sizeOf (error "reallocArray" :: a))

reallocArray0 :: forall a . Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 p n = reallocArray p (n + 1)

callocArray :: forall a . Storable a => Int -> IO (Ptr a)
callocArray size = callocBytes (size * sizeOf (error "callocArray" :: a))

callocArray0 :: forall a . Storable a => Int -> IO (Ptr a)
callocArray0 size  = callocArray (size + 1)

peekArray :: forall a . Storable a => Int -> Ptr a -> IO [a]
peekArray size ptr | size <= 0 = return []
                   | otherwise = f (size-1) []
  where
    f n acc | n < 0 = return acc
    f n acc = do e <- peekElemOff ptr n; f (n-1) (e:acc)

peekArray0 :: forall a . (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 marker ptr  = do
  size <- lengthArray0 marker ptr
  peekArray size ptr

pokeArray :: forall a . Storable a => Ptr a -> [a] -> IO ()
pokeArray ptr vals0 = go vals0 0
  where go [] _         = return ()
        go (val:vals) n = do { pokeElemOff ptr n val; go vals (n + 1) }

pokeArray0 :: forall a . Storable a => a -> Ptr a -> [a] -> IO ()
pokeArray0 marker ptr vals0 = go vals0 0
  where go []         n = pokeElemOff ptr n marker
        go (val:vals) n = do { pokeElemOff ptr n val; go vals (n + 1) }

newArray :: forall a . Storable a => [a] -> IO (Ptr a)
newArray vals  = do
  ptr <- mallocArray (length vals)
  pokeArray ptr vals
  return ptr

newArray0 :: forall a . Storable a => a -> [a] -> IO (Ptr a)
newArray0 marker vals  = do
  ptr <- mallocArray0 (length vals)
  pokeArray0 marker ptr vals
  return ptr

lengthArray0 :: forall a . (Storable a, Eq a) => a -> Ptr a -> IO Int
lengthArray0 marker ptr = loop 0
  where
    loop i = do
        val <- peekElemOff ptr i
        if val == marker then return i else loop (i+1)

withArray :: forall a b . Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray vals f = withArrayLen vals (const f)

withArray0 :: forall a b . Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 marker vals f = withArrayLen0 marker vals (const f)

withArrayLen :: forall a b . Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen vals f =
  allocaArray len $ \ptr -> do
    pokeArray ptr vals
    f len ptr
  where
    len = length vals

withArrayLen0 :: forall a b . Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 marker vals f =
  allocaArray0 len $ \ptr -> do
    pokeArray0 marker ptr vals
    f len ptr
  where
    len = length vals


advancePtr :: forall a. (Storable a) => Ptr a -> Int -> Ptr a
advancePtr p n = plusPtr p (n * sizeOf (error "advancePtr" :: a))

copyArray :: forall a. Storable a => Ptr a -> Ptr a -> Int -> IO ()
copyArray dest src n = copyBytes dest src (n * sizeOf (error "copyArray" :: a))

moveArray :: forall a. Storable a => Ptr a -> Ptr a -> Int -> IO ()
moveArray dest src n = moveBytes dest src (n * sizeOf (error "moveArray" :: a))
