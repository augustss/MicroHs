module Data.Array.ST_Type(module Data.Array.ST_Type) where
import Data.Array.IO(IOArray, IOUArray)

newtype STArray s i e = STArray (IOArray i e)

newtype STUArray s i e = STUArray (IOUArray i e)
