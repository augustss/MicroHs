module MicroHs.FFI(makeFFI) where
import Data.Function
import Data.List
import Data.Maybe
import MicroHs.Desugar(LDef)
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Ident
import MicroHs.Flags

makeFFI :: Flags -> [LDef] -> String
makeFFI _ ds =
  let ffiImports = [ (parseImpEnt (getSLoc t) f, t) | (_, Lit (LForImp f (CType t))) <- ds ]
      wrappers = [ t | (ImpWrapper, t) <- ffiImports]
      dynamics = [ t | (ImpDynamic, t) <- ffiImports]
      includes = "mhsffi.h" : catMaybes [ inc | (ImpStatic inc _addr _name, _) <- ffiImports ]
      addrs    = [ (name, t) | (ImpStatic _inc True  name, t) <- ffiImports ]
      funcs    = [ (name, t) | (ImpStatic _inc False name, t) <- ffiImports, name `notElem` runtimeFFI ]
      funcs'   = uniqFst funcs
      addrs'   = uniqFst addrs
  in
    if not (null wrappers) || not (null dynamics) then error "Unimplemented FFI feature" else
    unlines $
      map (\ fn -> "#include \"" ++ fn ++ "\"") includes ++
      map mkStatic funcs' ++
      map mkAddr   addrs' ++
      ["static struct ffi_entry table[] = {"] ++
      map (mkFuncEntry . fst) funcs' ++
      map (mkAddrEntry . fst) addrs' ++
      ["{ 0,0 }",
       "};",
       "struct ffi_entry *xffi_table = table;"
      ]

uniqFst :: [(String, EType)] -> [(String, EType)]
uniqFst = map head . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

data ImpEnt = ImpStatic (Maybe String) Bool String | ImpDynamic | ImpWrapper

-- "[static] [name.h] [&] [name]"
-- "dynamic"
-- "wrapper"
parseImpEnt :: SLoc -> String -> ImpEnt
parseImpEnt loc s =
  case words s of
    ["dynamic"] -> ImpDynamic
    ["wrapper"] -> ImpWrapper
    "static" : r -> rest r
    r            -> rest r
 where rest (inc : r) | isSuffixOf ".h" inc = rest' (ImpStatic (Just inc)) r
       rest r                               = rest' (ImpStatic Nothing)    r
       rest' c ("&" : r) = rest'' (c  True) r
       rest' c ['&' : r] = rest'' (c  True) [r]
       rest' c r         = rest'' (c False) r
       rest'' c [n] = c n
       rest'' _ _ = errorMessage loc $ "bad foreign import " ++ show s

mkFuncEntry :: String -> String
mkFuncEntry f = "{ \"" ++ f ++ "\", mhs_" ++ f ++ "},"

mkAddrEntry :: String -> String
mkAddrEntry f = "{ \"&" ++ f ++ "\", mhs_addr_" ++ f ++ "},"

iIO :: Ident
iIO = mkIdent "Primitives.IO"

iUnit :: Ident
iUnit = mkIdent "Primitives.()"

iPtr :: Ident
iPtr = mkIdent "Primitives.Ptr"

iFunPtr :: Ident
iFunPtr = mkIdent "Primitives.FunPtr"

mkStatic :: (String, EType) -> String
mkStatic (fn, t) =
  let !(as, ior) = getArrows t
      r = checkIO ior
      n = length as
      call = fn ++ "(" ++ intercalate ", " (zipWith mkArg as [0..]) ++ ")"
      fcall =
        if isUnit r then
          call ++ "; mhs_from_Unit(s, " ++ show n ++ ")"
        else
          mkRet r n call
  in  mkMhsFun fn fcall

mkMhsFun :: String -> String -> String
mkMhsFun fn body = "void mhs_" ++ fn ++ "(int s) { " ++ body ++ "; }"

checkIO :: EType -> EType
checkIO iot =
  case getApp iIO iot of
    Nothing -> errorMessage (getSLoc iot) $ "foreign return type must be IO: " ++ showEType iot
    Just t  -> t

getApp :: Ident -> EType -> Maybe EType
getApp i (EApp (EVar i') t) | i == i' = Just t
getApp _ _ = Nothing

isUnit :: EType -> Bool
isUnit (EVar unit) = unit == iUnit
isUnit _ = False

mkRet :: EType -> Int -> String -> String
mkRet t n call = "mhs_from_" ++ cTypeName t ++ "(s, " ++ show n ++ ", " ++ call ++ ")"

mkArg :: EType -> Int -> String
mkArg t i = "mhs_to_" ++ cTypeName t ++ "(s, " ++ show i ++ ")"

mkAddr :: (String, EType) -> String
mkAddr (fn, iot) =
  let r = checkIO iot
      (s, _) =
        case getApp iPtr r of
          Just t  -> ("", t)
          Nothing ->
            case getApp iFunPtr r of
              Just t  -> ("(HsFunPtr)", t)
              Nothing -> errorMessage (getSLoc r) $ "foreign & must be Ptr/FunPtr"
      body = mkRet r 0 (s ++ "&" ++ fn)
  in  mkMhsFun ("addr_" ++ fn) body

cTypeName :: EType -> String
cTypeName (EApp (EVar ptr) _t) | ptr == iPtr = "Ptr"
                               | ptr == iFunPtr = "FunPtr"
cTypeName (EVar i) | Just c <- lookup (unIdent i) cTypes = c
cTypeName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cTypes :: [(String, String)]
cTypes =
  -- These are temporary
  [ ("Primitives.FloatW", "FloatW")
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
  "md5Array", "md5BFILE", "md5String", "memcpy", "memmove", 
  "putb", "sin", "sqrt", "system", "tan", "tmpname", "ungetb", "unlink",
  "peekPtr", "pokePtr", "pokeWord", "peekWord",
  "add_lz77_compressor", "add_lz77_decompressor",
  "peek_uint8", "poke_uint8", "peek_uint16", "poke_uint16", "peek_uint32", "poke_uint32", "peek_uint64", "poke_uint64",
  "peek_int8", "poke_int8", "peek_int16", "poke_int16", "peek_int32", "poke_int32", "peek_int64", "poke_int64",
  "peek_ushort", "poke_ushort", "peek_short", "poke_short",
  "peek_uint", "poke_uint", "peek_int", "poke_int",
  "peek_ulong", "poke_ulong", "peek_long", "poke_long",
  "peek_ullong", "poke_ullong", "peek_llong", "poke_llong",
  "peek_flt", "poke_flt",
  "sizeof_int", "sizeof_long", "sizeof_llong",
  "opendir", "closedir", "readdir", "c_d_name", "chdir", "mkdir", "getcwd"
  ]
