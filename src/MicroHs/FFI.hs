module MicroHs.FFI(makeFFI) where
import qualified Prelude(); import MHSPrelude
import Data.Function
import Data.List
import MicroHs.Desugar(LDef)
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.Names
--import Debug.Trace

makeFFI :: Flags -> [String] -> [Ident] -> [LDef] -> (String, String)
makeFFI _ incs forExps ds =
  let ffiImports = [ (parseImpEnt cc i f, t) | (i, d) <- ds, Lit (LForImp cc f (CType t)) <- [get d] ]
                 where get (App _ a) = a   -- if there is no IO type, we have (App primPerform (LForImp ...))
                       get a = a
      wrappers = [ t | (ImpWrapper, t) <- ffiImports]
      dynamics = [ t | (ImpDynamic, t) <- ffiImports]
      imps     = uniqName $ filter ((`notElem` runtimeFFI) . impName) ffiImports
      includes = incs ++ nub [ inc | (ImpStatic _ (Just inc) _ _, _) <- imps ]
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

uniqName :: [(ImpEnt, EType)] -> [(ImpEnt, EType)]
uniqName = map head . groupBy ((==) `on` impName) . sortBy (compare `on` impName)

data ImpEnt = ImpStatic Ident (Maybe String) Imp String | ImpDynamic | ImpWrapper
--  deriving (Show)

impName :: (ImpEnt, EType) -> String
impName (ImpStatic i _ Value _, _) = unIdent' i
impName (ImpStatic _ _ _ s, _) = s
impName _ = undefined

data Imp = Ptr | Value | Func
--  deriving (Show)

-- "[static] [name.h] [&] [name]"
-- "dynamic"
-- "wrapper"
parseImpEnt :: CallConv -> Ident -> String -> ImpEnt
parseImpEnt _cc i s =
  case words s of
    ["dynamic"] -> ImpDynamic
    ["wrapper"] -> ImpWrapper
    "static" : r -> rest r
    r            -> rest r
 where rest (inc : r) | ".h" `isSuffixOf` inc = rest' (ImpStatic i (Just inc)) r
       rest r                                 = rest' (ImpStatic i Nothing)    r
       rest' c ("&"     : r) = rest'' (c Ptr) r
       rest' c ['&'     : r] = rest'' (c Ptr) [r]
       rest' c ("value" : r) = rest'' (c Value) [unwords r]
       rest' c r             = rest'' (c Func) r
       rest'' c [n] = c n
       rest'' _ _ = errorMessage loc $ "bad foreign import " ++ show s
       loc = getSLoc i

mkEntry :: (ImpEnt, EType) -> String
mkEntry (ImpStatic _ _ Func  f, t) = "{ \"" ++ f ++ "\", " ++ show (arity t) ++ ", mhs_" ++ f ++ "},"
mkEntry (ImpStatic _ _ Ptr   f, _) = "{ \"&" ++ f ++ "\", 0, mhs_addr_" ++ f ++ "},"
mkEntry (ImpStatic i _ Value _, _) = "{ \"" ++ f ++ "\", 0, mhs_" ++ f ++ "}," where f = unIdent' i
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

mkRet :: EType -> Int -> String -> String
mkRet t n call = "mhs_from_" ++ cTypeHsName t ++ "(s, " ++ show n ++ ", " ++ call ++ ")"

mkArg :: EType -> Int -> String
mkArg t i = "mhs_to_" ++ cTypeHsName t ++ "(s, " ++ show i ++ ")"

mkHdr :: (ImpEnt, EType) -> String
mkHdr (ImpStatic _ _ Ptr fn, iot) =
  let r = checkIO iot
      (s, _) =
        case dropApp identPtr r of
          Just t  -> ("", t)
          Nothing ->
            case dropApp identFunPtr r of
              Just t  -> ("(HsFunPtr)", t)
              Nothing -> errorMessage (getSLoc r) "foreign & must be Ptr/FunPtr"
      body = "return " ++ mkRet r 0 (s ++ "&" ++ fn)
  in  mkMhsFun ("addr_" ++ fn) body
mkHdr (ImpStatic _ _ Func fn, t) =
  let (as, ior) = getArrows t
      r = checkIO ior
      n = length as
      call = fn ++ "(" ++ intercalate ", " (zipWith mkArg as [0..]) ++ ")"
      fcall =
        if isUnit r then
          call ++ "; return mhs_from_Unit(s, " ++ show n ++ ")"
        else
          "return " ++ mkRet r n call
  in  mkMhsFun fn fcall
mkHdr (ImpStatic i _ Value val, iot) =
  let r = checkIO iot
      body = "return " ++ mkRet r 0 val
  in  mkMhsFun (unIdent' i) body
mkHdr _ = undefined

arity :: EType -> Int
arity = length . fst . getArrows

unIdent' :: Ident -> String
unIdent' = unIdent . unQualIdent

cTypeHsName :: EType -> String
cTypeHsName (EApp (EVar ptr) _t) | ptr == identPtr = "Ptr"
                               | ptr == identFunPtr = "FunPtr"
cTypeHsName (EVar i) | Just c <- lookup (unIdent i) cHsTypes = c
cTypeHsName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cHsTypes :: [(String, String)]
cHsTypes =
  -- These are temporary
  [ ("Primitives.FloatW", "FloatW")
  , ("Primitives.Int",    "Int")
  , ("Primitives.Word",   "Word")
  , ("Data.Word.Word8",   "Word8")
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
  [ ("Primitives.FloatW", "flt_t")
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

-- These are already in the runtime
runtimeFFI :: [String]
runtimeFFI = [
  "GETRAW", "GETTIMEMILLI", "acos", "add_FILE", "add_utf8", "asin", "atan", "atan2", "calloc", "closeb",
  "cos", "exp", "flushb", "fopen", "free", "getb", "getenv", "iswindows", "log", "malloc",
  "md5Array", "md5BFILE", "md5String", "memcpy", "memmove",
  "putb", "sin", "sqrt", "system", "tan", "tmpname", "ungetb", "unlink",
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
  "peek_flt", "poke_flt",
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
