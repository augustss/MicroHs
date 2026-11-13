module FFINameA(valueA) where

-- An import whose entity is not a C identifier as MicroHs sees it (it starts
-- with _) gets a C name from its position in the module.  FFINameB has one at
-- the same position: the names must not clash.
foreign import capi "ffiname.h value _ffiname_a" valueA :: Int
