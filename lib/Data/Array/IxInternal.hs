module Data.Array.IxInternal(module Data.Array.IxInternal) where
import Data.Ix

safeRangeSize :: Ix i => (i, i) -> Int
safeRangeSize (l,u) = let r = rangeSize (l, u)
                      in if r < 0 then error "Negative range size"
                                  else r

safeIndex :: Ix i => (i, i) -> Int -> i -> Int
safeIndex (l,u) n i = let i' = index (l,u) i
                      in if (0 <= i') && (i' < n)
                         then i'
                         else error ("Error in array index; " ++ show i' ++
                                     " not in range [0.." ++ show n ++ ")")
