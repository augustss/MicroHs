module Data.Array.Unboxed (
    UArray,
--    module Data.Array.IArray,
  ) where
--import Data.Array.IArray
import Data.ByteString.Internal

data UArray i e = UArray !i !i !Int ByteString
