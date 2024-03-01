module MicroHs.FFI(makeFFI) where
import Data.Function
import Data.List
import MicroHs.Desugar(LDef)
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Ident
import MicroHs.Flags

makeFFI :: Flags -> [LDef] -> String
makeFFI _ ds =
  let fs = [ (f, t) | (_, Lit (LForImp f (CType t))) <- ds, f `notElem` runtimeFFI ]
      fs' = map head $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) fs
  in
    -- "#include \"xffi.h\"\n" ++
    preamble ++
    unlines (map (uncurry mkWrapper) fs') ++
    "static struct ffi_entry table[] = {\n" ++
    unlines (map (mkEntry . fst) fs') ++
    "{ 0,0 }\n};\n" ++
    "struct ffi_entry *xffi_table = table;\n" ++
    "\n"

mkEntry :: String -> String
mkEntry f = "{ \"" ++ f ++ "\", mhs_" ++ f ++ "},"

mkWrapper :: String -> EType -> String
mkWrapper fn t =
  let (as, r) = getArrows t
      n = length as
      call = fn ++ "(" ++ intercalate ", " (zipWith mkArg as [0..]) ++ ")"
      vcall = mkRet r ++ "(s, " ++ show n ++ ", " ++ call ++ ")"
      fcall = if isIOUnit r then call ++ "; mhs_from_Unit(s, " ++ show n ++ ")" else vcall
  in  "void mhs_" ++ fn ++ "(int s) { " ++ fcall ++ "; }"

isIOUnit :: EType -> Bool
isIOUnit (EApp (EVar io) (EVar unit)) = io == mkIdent "Primitives.IO" && unit == mkIdent "Primitives.()"
isIOUnit _ = False

mkRet :: EType -> String
mkRet (EApp (EVar io) t) | io == mkIdent "Primitives.IO" = "mhs_from_" ++ cTypeName t
mkRet t = errorMessage (getSLoc t) $ "C return type is not IO: " ++ showEType t

mkArg :: EType -> Int -> String
mkArg t i = "mhs_to_" ++ cTypeName t ++ "(s, " ++ show i ++ ")"

cTypeName :: EType -> String
cTypeName (EApp (EVar ptr) _t) | ptr == mkIdent "Primitives.Ptr" = "Ptr"
cTypeName (EVar i) | Just c <- lookup (unIdent i) cTypes = c
cTypeName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cTypes :: [(String, String)]
cTypes =
  -- These are temporary
  [ ("Primitives.Double", "Double")
  , ("Primitives.Int",    "Int")
  , ("Primitives.Word",   "Word")
  , ("Data.Word.Word8",   "Word8")
  , ("Primitives.()",     "Unit")
  , ("System.IO.Handle",  "Ptr")
  ] ++ map (\ t -> ("Foreign.C.Types." ++ t, t))
  [ "CChar",
    "CSChar",
    "CUChar",
    "CShort",
    "CUShort",
    "CInt",
    "CUInt",
    "CLong",
    "CULong",
    "CPtrdiff",
    "CSize",
    "CSSize",
    "CLLong",
    "CULLong"
  ]

-- These are already in the runtime
runtimeFFI :: [String]
runtimeFFI = [
  "GETRAW", "GETTIMEMILLI", "acos", "add_FILE", "add_utf8", "asin", "atan", "atan2", "calloc", "closeb",
  "cos", "exp", "flushb", "fopen", "free", "getb", "getenv", "iswindows", "log", "lz77c", "malloc",
  "md5Array", "md5BFILE", "md5String", "memcpy", "memmove", "peekByte", "peekPtr", "peekWord", "pokeByte",
  "pokePtr", "pokeWord", "putb", "sin", "sqrt", "system", "tan", "tmpname", "unlink"
  ]

-- Avoid includes for now
preamble :: String
preamble = "\
\typedef void (*funptr_t)(int);\n\
\struct ffi_entry {\n\
\  const char *ffi_name;\n\
\  funptr_t ffi_fun;\n\
\};\n\
\"
