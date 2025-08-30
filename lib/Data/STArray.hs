-- Temporary module
module Data.STArray(module Data.STArray) where
import Primitives(primPerformIO)
import Control.Monad.ST
import Control.Monad.ST_Type
import Data.Array
import Data.IOArray

newtype STArray s a = A (IOArray a)

newSTArray :: Int -> a -> ST s (STArray s a)
newSTArray n a = ST (A <$> newIOArray n a)

sizeSTArray :: STArray s a -> Int
sizeSTArray (A a) = primPerformIO (sizeIOArray a)

readSTArray :: STArray s a -> Int -> ST s a
readSTArray (A a) i = ST (readIOArray a i)

writeSTArray :: STArray s a -> Int -> a -> ST s ()
writeSTArray (A a) i x = ST (writeIOArray a i x)

freezeSTArray :: STArray s a -> ST s (Array Int a)
freezeSTArray (A a) = ST (freezeIOArray a)
