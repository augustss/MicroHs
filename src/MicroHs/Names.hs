-- Copyright 2025 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Names(module MicroHs.Names) where
import qualified Prelude(); import MHSPrelude
import MicroHs.Expr
import MicroHs.Ident

-- Identifiers known by the compiler.

builtinLoc :: SLoc
builtinLoc = SLoc "builtin" 0 0

mkIdentB :: String -> Ident
mkIdentB = mkIdentSLoc builtinLoc

boolPrefix :: String
boolPrefix = "Data.Bool_Type."

listPrefix :: String
listPrefix = "Data.List_Type."

nameList :: String
nameList = listPrefix ++ "[]"
identList :: Ident
identList = mkIdentB nameList

nameInt :: String
nameInt = "Primitives.Int"
identInt :: Ident
identInt = mkIdentB nameInt

nameInt64 :: String
nameInt64 = "Primitives.Int64"
identInt64 :: Ident
identInt64 = mkIdentB nameInt64

nameWord :: String
nameWord = "Primitives.Word"
identWord :: Ident
identWord = mkIdentB nameWord

nameWord64 :: String
nameWord64 = "Primitives.Word64"
identWord64 :: Ident
identWord64 = mkIdentB nameWord64

nameFloat :: String
nameFloat = "Primitives.Float"
identFloat :: Ident
identFloat = mkIdentB nameFloat

nameDouble :: String
nameDouble = "Primitives.Double"
identDouble :: Ident
identDouble = mkIdentB nameDouble

nameChar :: String
nameChar = "Primitives.Char"
identChar :: Ident
identChar = mkIdentB nameChar

nameInteger :: String
nameInteger = "Data.Integer_Type.Integer"
identInteger :: Ident
identInteger = mkIdentB nameInteger

nameTypeEq :: String
nameTypeEq = "Primitives.~"
identTypeEq :: Ident
identTypeEq = mkIdentB nameTypeEq

nameImplies :: String
nameImplies = "Primitives.=>"
identImplies :: Ident
identImplies = mkIdentB nameImplies

nameArrow :: String
nameArrow = "Primitives.->"
identArrow :: Ident
identArrow = mkIdentB nameArrow

nameSymbol :: String
nameSymbol = "Primitives.Symbol"

nameNat :: String
nameNat = "Primitives.Nat"

nameType :: String
nameType = "Primitives.Type"

nameKind :: String
nameKind = "Primitives.Kind"

nameConstraint :: String
nameConstraint = "Primitives.Constraint"

nameKnownNat :: String
nameKnownNat = "Data.TypeLits.KnownNat"

nameKnownSymbol :: String
nameKnownSymbol = "Data.TypeLits.KnownSymbol"

nameDataTypeableTypeable :: String
nameDataTypeableTypeable = "Data.Typeable.Typeable"
identDataTypeableTypeable :: Ident
identDataTypeableTypeable = mkIdentB nameDataTypeableTypeable

nameCoercible :: String
nameCoercible = "Data.Coerce.Coercible"

nameHasField :: String
nameHasField = "Data.Records.HasField"

nameSetField :: String
nameSetField = "Data.Records.SetField"

namegetField :: String
namegetField = "getField"

namesetField :: String
namesetField = "setField"

nameByteString :: String
nameByteString = "Data.ByteString.Internal.ByteString"
identByteString :: Ident
identByteString = mkIdentB nameByteString

identIO :: Ident
identIO = mkIdentB "Primitives.IO"

identUnit :: Ident
identUnit = mkIdentB "()"

identPtr :: Ident
identPtr = mkIdentB "Primitives.Ptr"

identFunPtr :: Ident
identFunPtr = mkIdentB "Primitives.FunPtr"

-- The newtypes for C types, each with its C type and the primitive type
-- that represents it (which must match the newtype's definition).
-- The type of a foreign import keeps these newtypes, while all others
-- are expanded, because a call through a function pointer needs the
-- exact C types.
cNewtypes :: [(String, (String, String))]
cNewtypes =
  [ (c "CBool",    ("uint8_t",            w))
  , (c "CChar",    ("char",               i))
  , (c "CSChar",   ("signed char",        i))
  , (c "CUChar",   ("unsigned char",      w))
  , (c "CWchar",   ("wchar_t",            i))
  , (c "CShort",   ("short",              i))
  , (c "CUShort",  ("unsigned short",     w))
  , (c "CInt",     ("int",                i))
  , (c "CUInt",    ("unsigned int",       w))
  , (c "CLong",    ("long",               i))
  , (c "CULong",   ("unsigned long",      w))
  , (c "CPtrdiff", ("ptrdiff_t",          w))
  , (c "CSize",    ("size_t",             w))
  , (c "CSSize",   ("intptr_t",           i))   -- ssize_t is not standard C
  , (c "CLLong",   ("long long",          i64))
  , (c "CULLong",  ("unsigned long long", w64))
  , (c "CIntPtr",  ("intptr_t",           i))
  , (c "CUIntPtr", ("uintptr_t",          w))
  , (c "CIntMax",  ("intmax_t",           i64))
  , (c "CUIntMax", ("uintmax_t",          w64))
  , (c "CClock",   ("clock_t",            i))
  , (c "CTime",    ("time_t",             i))
  , (c "CFloat",   ("float",              "Primitives.Float"))
  , (c "CDouble",  ("double",             "Primitives.Double"))
  , ("Data.Int.Int8.Int8",      ("int8_t",   i))
  , ("Data.Int.Int16.Int16",    ("int16_t",  i))
  , ("Data.Int.Int32.Int32",    ("int32_t",  i))
  , ("Data.Word.Word8.Word8",   ("uint8_t",  w))
  , ("Data.Word.Word16.Word16", ("uint16_t", w))
  , ("Data.Word.Word32.Word32", ("uint32_t", w))
  ]
  where c s = "Foreign.C.Types." ++ s
        i   = "Primitives.Int"
        w   = "Primitives.Word"
        i64 = "Primitives.Int64"
        w64 = "Primitives.Word64"

identStablePtr :: Ident
identStablePtr = mkIdentB "Foreign.StablePtr.StablePtr"

identNil :: Ident
identNil = mkIdent $ listPrefix ++ "[]"

identCons :: Ident
identCons = mkIdent $ listPrefix ++ ":"

-----

conCons :: Expr
conCons = ECon $ ConData conTyInfoList identCons []

conNil :: Expr
conNil = ECon $ ConData conTyInfoList identNil []

conTyInfoList :: ConTyInfo
conTyInfoList = [(identNil, 0), (identCons, 2)]

tupleCon :: SLoc -> Int -> Expr
tupleCon loc n =
  let
    c = tupleConstr loc n
  in ECon $ ConData [(c, n)] c []

-----

instPrefix :: String
instPrefix = "inst"

uniqIdentSep :: String
uniqIdentSep = "$"

-- Needed dictionaries
dictPrefix :: String
dictPrefix = "dict"

dictPrefixDollar :: String
dictPrefixDollar = dictPrefix ++ uniqIdentSep

-- Dictionary argument names
adictPrefix :: String
adictPrefix = "adict"
