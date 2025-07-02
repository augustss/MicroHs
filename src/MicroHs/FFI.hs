module MicroHs.FFI(makeFFI) where
import qualified Prelude(); import MHSPrelude
import Data.List
import GHC.Stack
import MicroHs.Desugar(LDef)
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.Names
--import Debug.Trace

makeFFI :: Flags -> [Ident] -> [LDef] -> (String, String)
makeFFI _ forExps ds =
  let ffiImports = nubBy eq [ (ie, n, t) | (_, d) <- ds, Lit (LForImp ie n (CType t)) <- [get d] ]
                 where get (App _ a) = a   -- if there is no IO type, we have (App primPerform (LForImp ...))
                       get a = a
                       eq (_, n, _) (_, n', _) = n == n'
      wrappers = [ t | (ImpWrapper, _, t) <- ffiImports]
      dynamics = [ t | (ImpDynamic, _, t) <- ffiImports]
      imps     = filter ((`notElem` runtimeFFI) . impName) ffiImports
      includes = jsincs ++ nub [ inc | (ImpStatic iincs _ _, _, _) <- imps, inc <- iincs ]
      jsincs   = if any isJS ffiImports then ["emscripten.h"] else []
        where isJS (ImpJS _, _, _) = True
              isJS _ = False
      exps     = [ (i, t) | (i, e) <- ds, Just (_, t) <- [getForExp e] ]
      mkSig (i, CType t) = let (as, ior) = getArrows t in mkExportSig i as ior ++ ";"
      header = unlines
        ["#include <stdint.h>",
         "#if defined(__cplusplus)",
         "extern \"C\" {",
         "#endif",
         "void mhs_init(void);",
         intercalate "\n" $ map mkSig exps,
         "#if defined(__cplusplus)",
         "}",
         "#endif"
        ]
  in
    if not (null wrappers) || not (null dynamics) then error "Unimplemented FFI feature" else
    (unlines $
      map (\ fn -> "#include \"" ++ fn ++ "\"") includes ++
      map mkHdr imps ++
      ["static struct ffi_entry imp_table[] = {"] ++
      map mkEntry imps ++
      ["{ 0,0 }",
       "};",
       "struct ffi_entry *xffi_table = imp_table;"
      ] ++
      ["static struct ffe_entry exp_table[] = {"] ++
      map mkExport forExps ++
      ["  { 0,0 }",
       "};",
       "struct ffe_entry *xffe_table = exp_table;",
       "\n"
      ] ++ zipWith mkExportWrapper [0..] exps
    , header)

mkExportSig :: Ident -> [EType] -> EType -> String
mkExportSig n as ior =
  let outT = cTypeName $ checkIO ior
      ins = zipWith (\ i a -> cTypeName a ++ " _x" ++ show i) [1::Int ..] as
   in outT ++ " " ++ unIdent n ++ "(" ++ intercalate ", " ins ++ ")"

mkExport :: Ident -> String
mkExport i = "  { \"" ++ unIdent i ++ "\", 0 },"

mkExportWrapper :: Int -> (Ident, CType) -> String
mkExportWrapper no (n, CType t) = unlines $
  let (as, ior) = getArrows t
      r = checkIO ior
      outT = cTypeName r
      arg k a = "  mhs_from_" ++ cTypeHsName a ++ "(ffe_alloc(), 0, _x" ++ show k ++ "); ffe_apply();"
      eval = if eqEType r ior then "ffe_eval()" else "ffe_exec()"
  in  [mkExportSig n as ior ++ " {",
       "  gc_check(" ++ show (2 * length as + 4) ++ ");",
       "  ffe_push(xffe_table[" ++ show no ++ "].ffe_value);" ]
      ++ zipWith arg [1::Int ..] as ++
      if isUnit r then
        [ "  (void)" ++ eval ++ ";",
          "  ffe_pop();",
          "}"
        ]
       else
        [ "  " ++ outT ++ " _res = mhs_to_" ++ cTypeHsName r ++ "(" ++ eval ++ ", -1);",
          "  ffe_pop();",
          "  return _res;",
          "}"
        ]

impName :: (ImpEnt, String, EType) -> String
impName (_, s, _) = s

mkEntry :: (ImpEnt, String, EType) -> String
mkEntry (ImpStatic _ IFunc  _, f, t) = "{ \"" ++ f ++ "\", " ++ show (arity t) ++ ", mhs_" ++ f ++ "},"
mkEntry (ImpStatic _ IPtr   _, f, _) = "{ \"&" ++ f ++ "\", 0, mhs_addr_" ++ f ++ "},"
mkEntry (ImpStatic _ IValue _, f, _) = "{ \"" ++ f ++ "\", 0, mhs_" ++ f ++ "},"
mkEntry (ImpJS _,              f, t) = "{ \"" ++ f ++ "\", " ++ show (arity t) ++ ", mhs_" ++ f ++ "},"
mkEntry _ = undefined

mkMhsFun :: String -> String -> String
mkMhsFun fn body = "from_t mhs_" ++ fn ++ "(int s) { " ++ body ++ "; }"

checkIO :: EType -> EType
checkIO iot =
  case dropApp identIO iot of
    Nothing -> iot -- errorMessage (getSLoc iot) $ "foreign return type must be IO: " ++ showEType iot
    Just t  -> t

dropApp :: Ident -> EType -> Maybe EType
dropApp i (EApp (EVar i') t) | i == i' = Just t
dropApp _ _ = Nothing

isUnit :: EType -> Bool
isUnit (EVar unit) = unit == identUnit
isUnit _ = False

mkRet :: HasCallStack => EType -> Int -> String -> String
mkRet t n call = "mhs_from_" ++ cTypeHsName t ++ "(s, " ++ show n ++ ", " ++ call ++ ")"

mkArg :: EType -> Int -> String
mkArg t i = "mhs_to_" ++ cTypeHsName t ++ "(s, " ++ show i ++ ")"

mkJSArg :: EType -> Int -> String
mkJSArg t i = "mhs_to_" ++ jsTypeName t ++ "(s, " ++ show i ++ ")"

mkHdr :: (ImpEnt, String, EType) -> String
mkHdr (ImpStatic _ IPtr fn, f, iot) =
  let r = checkIO iot
      (s, _) =
        case dropApp identPtr r of
          Just t  -> ("", t)
          Nothing ->
            case dropApp identFunPtr r of
              Just t  -> ("(HsFunPtr)", t)
              Nothing -> errorMessage (getSLoc r) "foreign & must be Ptr/FunPtr"
      body = "return " ++ mkRet r 0 (s ++ "&" ++ fn)
  in  mkMhsFun ("addr_" ++ f) body
mkHdr (ImpStatic _ IFunc fn, f, t) =
  let (as, ior) = getArrows t
      r = checkIO ior
      n = length as
      call = fn ++ "(" ++ intercalate ", " (zipWith mkArg as [0..]) ++ ")"
      fcall =
        if isUnit r then
          call ++ "; return mhs_from_Unit(s, " ++ show n ++ ")"
        else
          "return " ++ mkRet r n call
  in  mkMhsFun f fcall
mkHdr (ImpStatic _ IValue val, f, iot) =
  let r = checkIO iot
      body = "return " ++ mkRet r 0 val
  in  mkMhsFun f body
mkHdr (ImpJS s, f, ty) =
  let (as, ior) = getArrows ty
      rt = checkIO ior
      jsr = jsTypeNameR rt
      n = length as
      args = concat $ zipWith arg as [0..]
      arg t i = ", " ++ mkJSArg t i
      call = "EM_ASM" ++
             (if isUnit rt then "" else '_':jsr) ++
             "({ " ++ s ++ " }" ++ args ++ ")"
      fcall =
        if isUnit rt then
          call ++ "; return mhs_from_Unit(s, " ++ show n ++ ")"
        else
          "return " ++ mkRet rt n call
  in  mkMhsFun f fcall
mkHdr _ = undefined

arity :: EType -> Int
arity = length . fst . getArrows

cTypeHsName :: HasCallStack => EType -> String
cTypeHsName (EApp (EVar ptr) _t) | ptr == identPtr = "Ptr"
                               | ptr == identFunPtr = "FunPtr"
cTypeHsName (EVar i) | Just c <- lookup (unIdent i) cHsTypes = c
cTypeHsName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cHsTypes :: [(String, String)]
cHsTypes =
  -- These are temporary
  [ ("Primitives.Float",  "Float")
  , ("Primitives.Double", "Double")
  , ("Primitives.Int",    "Int")
  , ("Primitives.Word",   "Word")
  , ("Data.Word.Word8",   "Word8")
  , ("Data.Word.Word16",  "Word16")
  , ("Data.Word.Word32",  "Word32")
  , ("Data.Word.Word64",  "Word64")
  , ("Data.Int.IntN.Int8","Int8")
  , ("Data.Int.IntN.Int16","Int16")
  , ("Data.Int.IntN.Int32","Int32")
  , ("Data.Int.IntN.Int64","Int64")
  , ("()",                "Unit")
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
    "CULLong",
    "CTime"
  ]

cTypeName :: EType -> String
cTypeName (EApp (EVar ptr) t) | ptr == identPtr = cTypeName t ++ "*"
cTypeName (EVar i) | Just c <- lookup (unIdent i) cTypes = c
cTypeName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cTypes :: [(String, String)]
cTypes =
  -- These are temporary
  [ ("Primitives.Float",  "flt32_t")
  , ("Primitives.Double", "flt64_t")
  , ("Primitives.Int",    "intptr_t")   -- value_t
  , ("Primitives.Word",   "uintptr_t")  -- uvalue_t
  , ("Data.Word.Word8",   "uint8_t")
  , ("()",                "void")
  , ("System.IO.Handle",  "void*")
  ] ++ map (first ("Foreign.C.Types." ++))
  [ ("CChar", "char"),
    ("CSChar", "signed char"),
    ("CUChar", "unsigned char"),
    ("CShort", "short"),
    ("CUShort", "unsigned short"),
    ("CInt", "int"),
    ("CUInt", "unsigned int"),
    ("CLong", "long"),
    ("CULong", "unsigned long"),
    ("CPtrdiff", "ptrdiff_t"),
    ("CSize", "size_t"),
    ("CSSize", "ssize_t"),
    ("CLLong", "long long"),
    ("CULLong", "unsigned long long"),
    ("CTime", "time_t")
  ]

jsTypeNameR :: EType -> String
jsTypeNameR (EApp (EVar ptr) _) | ptr == identPtr = "PTR"
jsTypeNameR (EVar i) | Just c <- lookup (unIdent i) jsTypesR = c
jsTypeNameR t = errorMessage (getSLoc t) $ "Not a valid Javascript return type: " ++ showEType t

jsTypesR :: [(String, String)]
jsTypesR =
  [ ("Primitives.Int",    "INT")
  , ("Primitives.Double", "DOUBLE")
  ]

jsTypeName :: EType -> String
jsTypeName (EApp (EVar ptr) _) | ptr == identPtr = "Ptr"
jsTypeName (EVar i) | Just c <- lookup (unIdent i) jsTypes = c
jsTypeName t = errorMessage (getSLoc t) $ "Not a valid Javascript return type: " ++ showEType t

jsTypes :: [(String, String)]
jsTypes =
  [ ("Primitives.Int",    "Int")
  , ("Primitives.Double", "Double")
  ]

-- These are already in the runtime
runtimeFFI :: [String]
runtimeFFI = [
  "GETRAW", "GETTIMEMILLI", "acos", "add_FILE", "add_utf8", "asin", "atan", "atan2", "calloc", "closeb",
  "cos", "exp", "flushb", "fopen", "free", "getb", "getenv", "iswindows", "log", "malloc",
  "md5Array", "md5BFILE", "md5String", "memcpy", "memmove", "strlen", "strcpy",
  "putb", "sin", "sqrt", "system", "tan", "tmpname", "ungetb", "unlink",
  "acosf", "asinf", "atanf", "atan2f", "cosf", "expf", "logf", "sinf", "sqrtf", "tanf",
  "scalbn", "scalbnf",
  "readb", "writeb",
  "peekPtr", "pokePtr", "pokeWord", "peekWord",
  "add_lz77_compressor", "add_lz77_decompressor",
  "add_rle_compressor", "add_rle_decompressor",
  "add_bwt_compressor", "add_bwt_decompressor",
  "peek_uint8", "poke_uint8", "peek_uint16", "poke_uint16", "peek_uint32", "poke_uint32", "peek_uint64", "poke_uint64",
  "peek_int8", "poke_int8", "peek_int16", "poke_int16", "peek_int32", "poke_int32", "peek_int64", "poke_int64",
  "peek_ushort", "poke_ushort", "peek_short", "poke_short",
  "peek_uint", "poke_uint", "peek_int", "poke_int",
  "peek_ulong", "poke_ulong", "peek_long", "poke_long",
  "peek_ullong", "poke_ullong", "peek_llong", "poke_llong",
  "peek_size_t", "poke_size_t",
  "peek_flt32", "poke_flt32",
  "peek_flt64", "poke_flt64",
  "sizeof_int", "sizeof_long", "sizeof_llong", "sizeof_size_t",
  "opendir", "closedir", "readdir", "c_d_name", "chdir", "mkdir", "getcwd",
  "getcpu",
  "get_buf", "openb_rd_buf", "openb_wr_buf",
  "new_mpz", "mpz_abs", "mpz_add", "mpz_and", "mpz_cmp", "mpz_get_d",
  "mpz_get_si", "mpz_get_ui", "mpz_init_set_si", "mpz_init_set_ui", "mpz_ior",
  "mpz_mul", "mpz_mul_2exp", "mpz_neg", "mpz_popcount", "mpz_sub", "mpz_fdiv_q_2exp",
  "mpz_tdiv_qr", "mpz_tstbit", "mpz_xor",
  "want_gmp"
  ]
