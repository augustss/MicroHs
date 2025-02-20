module Data.Array.ST (
   STArray,
   runSTArray,
   STUArray,
   runSTUArray,
   module Data.Array.MArray,
 ) where
import Control.Monad.ST
import Data.Array(Array)
import Data.Array.IO
import Data.Array.MArray_Class
import Data.Array.ST_Type
import Data.Array.Unboxed(UArray)

runSTArray :: (forall s . ST s (STArray s i e)) -> Array i e
runSTArray st = runST (st >>= unsafeFreezeSTArray)

runSTUArray :: (forall s . ST s (STUArray s i e)) -> UArray i e
runSTUArray st = runST (st >>= unsafeFreezeSTUArray)
