module FFINameB(valueB) where

-- See FFINameA.
foreign import capi "ffiname.h value _ffiname_b" valueB :: Int
