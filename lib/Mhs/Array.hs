module Mhs.Array(
  module Data.Ix,
  Array(..),
  array,
  listArray,
  accumArray,
  (!),
  bounds,
  indices,
  elems,
  assocs,
  (//),
  accum,
  ixmap,
  -----
  numElements,
  safeRangeSize,
  safeIndex,
  unsafeArray,
  unsafeAt,
  unsafeReplace,
  unsafeAccum,
--  freezeIOArray,
  ) where
import qualified Prelude()
import Primitives(primPerformIO, primArrCopy, primArrEQ)
import Control.Error
import Control.Monad
import Data.Bool
import Data.Char
import Data.Enum
import Data.Eq
import Data.Foldable(Foldable(..))
import Data.Function
import Data.Functor
import Data.Int.Int
import Data.Ix
import Data.List as List
import Data.Num
import Data.Ord
import Data.Traversable(Traversable(traverse))
import Data.Tuple
import Mhs.MutArr
import {-# SOURCE #-} Data.Typeable
import System.IO.Base
import Text.ParserCombinators.ReadPrec
import Text.Read.Internal
import qualified Text.Read.Lex as L
import Text.Show

data Array i a
   = Array (i,i)        -- bounds
           !Int         -- = (rangeSize (l,u))
           (MutIOArr a) -- elements

instance Ix a => Functor (Array a) where
  fmap f a@(Array b _ _) = array b [(i, f (a ! i)) | i <- range b]

instance (Ix a, Eq b)  => Eq (Array a b) where
  (==) (Array b1 _ a1) (Array b2 _ a2) = b1 == b2 && primArrEQ a1 a2

instance (Ix a, Ord b) => Ord (Array a b) where
  compare arr1 arr2 = compare (assocs arr1) (assocs arr2)

instance Foldable (Array i) where
  foldr f b0 arr@(Array _ n _) =
    let
      go i | i == n = b0
           | e <- unsafeAt arr i = f e (go (i + 1))
    in go 0
  foldl f b0 arr@(Array _ n _) =
    let
      go i | i == -1 = b0
           | e <- unsafeAt arr i = f (go (i - 1)) e
    in go (n - 1)
  foldr' f b0 arr@(Array _ n _) =
    let
      go i a | i == -1 = a
             | e <- unsafeAt arr i = go (i - 1) (f e $! a)
    in go (n - 1) b0
  foldl' f b0 arr@(Array _ n _) =
    let
      go i a | i == n = a
             | e <- unsafeAt arr i = go (i + 1) (a `seq` f a e)
    in go 0 b0
  foldl1 f arr@(Array _ n _) =
    let
      go i | i == 0 = unsafeAt arr 0
           | e <- unsafeAt arr i = f (go (i - 1)) e
    in if n == 0 then error "foldl1: empty Array" else go (n - 1)
  foldr1 f arr@(Array _ n _) =
    let
      go i | i == n - 1 = unsafeAt arr i
           | e <- unsafeAt arr i = f e (go (i + 1))
    in if n == 0 then error "foldr1: empty Array" else go 0
  toList = elems
  length = numElements
  null a = numElements a == 0

instance Ix i => Traversable (Array i) where
  traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

instance (Ix a, Show a, Show b) => Show (Array a b) where
  showsPrec p a =
    showParen (p > appPrec) $
    showString "array " .
    showsPrec appPrec1 (bounds a) .
    showChar ' ' .
    showsPrec appPrec1 (assocs a)

instance (Ix a, Read a, Read b) => Read (Array a b)  where
    readPrec = parens $ prec appPrec $
               do expectP (L.Ident "array")
                  theBounds <- step readPrec
                  vals   <- step readPrec
                  return (array theBounds vals)

    readListPrec = readListPrecDefault
    readList     = readListDefault

array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b ies =
  let n = safeRangeSize b
  in  unsafeArray' b n [(safeIndex b n i, e) | (i, e) <- ies]

listArray  :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b es =
  let n = safeRangeSize b
  in  if List.length es > n then error "listArray: list too long" else unsafeArray' b n (zip [0..] es)

accumArray :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
accumArray f z b = accum f (array b [(i, z) | i <- range b])

(!) :: (Ix a) => Array a b -> a -> b
(!) arr@(Array b n _) i = unsafeAt arr (safeIndex b n i)

unsafeAt :: Array a b -> Int -> b
unsafeAt (Array _ _ a) i = primPerformIO $ unsafeReadMutIOArr a i

bounds :: Array a b -> (a,a)
bounds (Array b _ _) = b

numElements :: Array a b -> Int
numElements (Array _ n _) = n

indices :: (Ix a) => Array a b -> [a]
indices (Array b _ _) = range b

elems :: Array a b -> [b]
elems (Array _ _ a) = primPerformIO $ elemsIOArr a

assocs :: (Ix a) => Array a b -> [(a,b)]
assocs a = zip (indices a) (elems a)

(//) :: (Ix a) => Array a b -> [(a,b)] -> Array a b
(//) arr@(Array b n _) ies = unsafeReplace arr [(safeIndex b n i, e) | (i, e) <- ies ]

unsafeReplace :: Array a b -> [(Int,b)] -> Array a b
unsafeReplace (Array b n oa) ies = primPerformIO $ do
  a <- primArrCopy oa
  let adj (i, e) = unsafeWriteMutIOArr a i e
  mapM_ adj ies
  return $ Array b n a

accum :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
accum f arr@(Array b n _) ies = unsafeAccum f arr [(safeIndex b n i, e) | (i, e) <- ies]

ixmap :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c
ixmap b f a = array b [(i, a ! f i) | i <- range b]

-------

unsafeAccum :: (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum f (Array b n oa) ies = primPerformIO $ do
  a <- primArrCopy oa
  let adj (i, e) = do
        x <- unsafeReadMutIOArr a i
        let x' = f x e
        seq x' (unsafeWriteMutIOArr a i x')
  mapM_ adj ies
  return $ Array b n a

unsafeArray :: Ix i => (i,i) -> [(Int, e)] -> Array i e
unsafeArray lu ies = unsafeArray' lu (unsafeRangeSize lu) ies

unsafeArray' :: (i,i) -> Int -> [(Int, e)] -> Array i e
unsafeArray' b n ies = primPerformIO $ do
  a <- newMutIOArr n arrEleBottom
  mapM_ (uncurry (unsafeWriteMutIOArr a)) ies
  return $ Array b n a

arrEleBottom :: a
arrEleBottom = error "(Array.!): undefined array element"

safeIndex :: Ix i => (i, i) -> Int -> i -> Int
safeIndex (l,u) n i | 0 <= i' && i' < n = i'
                    | otherwise         = badSafeIndex i' n
  where i' = index (l,u) i

badSafeIndex :: Int -> Int -> a
badSafeIndex i n = error $ ("Error in array index; "::String) ++ show i ++ (" not in range [0.."::String) ++ show n ++ (")"::String)

safeRangeSize :: Ix i => (i, i) -> Int
safeRangeSize b =
  let r = rangeSize b
  in  if r < 0 then error "Negative range size" else r

elemsIOArr :: forall a . MutIOArr a -> IO [a]
elemsIOArr a = do
  s <- sizeMutIOArr a
  mapM (unsafeReadMutIOArr a) [0::Int .. s - 1]

{-
freezeIOArray :: IOArray a -> IO (Array Int a)
freezeIOArray ioa = do
  s <- sizeIOArray ioa
  ioa' <- primArrCopy ioa
  return $ Array (0, s-1) s ioa'
-}
