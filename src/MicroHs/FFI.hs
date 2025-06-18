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

makeFFI :: Flags -> [EType] -> [LDef] -> String
makeFFI _ es ds =
  let ffiImports = [ (parseImpEnt i f, t) | (i, d) <- ds, Lit (LForImp f (CType t)) <- [get d] ]
                 where get (App _ a) = a   -- if there is no IO type, we have (App primPerform (LForImp ...))
                       get a = a
      wrappers = [ t | (ImpWrapper, t) <- ffiImports]
      dynamics = [ t | (ImpDynamic, t) <- ffiImports]
      imps     = uniqName $ filter ((`notElem` runtimeFFI) . impName) ffiImports
      includes = "mhsffi.h" : nub [ inc | (ImpStatic _ (Just inc) _ _, _) <- imps ]
      exps     = filter (\(i, _) -> "$exp$" `isPrefixOf` unIdent i) ds
  in
    if not (null wrappers) || not (null dynamics) then error "Unimplemented FFI feature" else
    unlines $
      map (\ fn -> "#include \"" ++ fn ++ "\"") includes ++
      map mkHdr imps ++
      ["static struct ffi_entry imp_table[] = {"] ++
      map mkEntry imps ++
      ["{ 0,0 }",
       "};",
       "struct ffi_entry *xffi_table = imp_table;"
      ] ++
      ["static struct ffe_entry exp_table[] = {"] ++
      map mkExport exps ++
      ["{ 0,0 }",
       "};",
       "struct ffe_entry *xffe_table = exp_table;",
       "\n// Foreign export wrappers:"
      ] ++ map mkExportWrapper (zip3 [0..] es exps)

mkExport :: LDef -> String
mkExport (_, e) = "  { \"" ++ exportDeclName e ++ "\", 0 }"

-- | Get the foreign export declaration name, when showed it is: 'ForExp.ofuncName'
exportDeclName :: Exp -> String
exportDeclName e = drop (length "ForExp.") (show e)

exportDeclHeader :: Ident -> EType -> String
exportDeclHeader i t = output ++ " " ++ name ++ "(" ++ inputs ++ ")"
  where
    (inputsE, outputE) = getArrowsOper t
    output = cfTypeName (checkIO outputE)
    inputs = intercalate ", " (zipWith mkParam inputsE [0..])
    name = drop (length "$exp$") (unIdent i)

-- TODO: we should be using getArrows, but the expr we have for foreign export is an EOper.
getArrowsOper :: EType -> ([EType], EType)
getArrowsOper (EOper x xs) = (init args, last args)
  where
    args = x : [e | (ap, e) <- xs, isIdent "->" ap]
getArrowsOper x = ([], x)

mkExportWrapper :: (Int, EType, LDef) -> String
mkExportWrapper (k, t, (i, e)) = unlines $
  [exportDeclHeader i t ++ " { // " ++ show t] ++
  map (mappend "  ") body ++
  ["}"]
  where
    (inputsE, outputE) = getArrowsOper t
    output = cTypeName' (checkIO outputE)
    args = zipWith mkArgNode inputsE [0..]
    inputCount = length inputsE
    aps = take inputCount $ map (mkApNode k) [0..]
    (finalCount, performio) = case dropApp identIO outputE of
      Nothing -> (inputCount, [])
      Just _ -> (inputCount + 1, ["NODEPTR " ++ last_ap ++ " = new_app(alloc_node(T_PERFORMIO), p2);"])
    last_ap = "p" ++ show (inputCount - 1)
    body = args ++ aps ++ performio ++
      ["push(" ++ last_ap ++ ");",
       "NODEPTR res = evali(" ++ last_ap ++ ");",
       "pop(1); /* maybe, I'm not sure */",
       "return mhs_to_" ++ output ++ "(res);"
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
parseImpEnt :: Ident -> String -> ImpEnt
parseImpEnt i s =
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
mkRet t n call = "mhs_from_" ++ cTypeName t ++ "(s, " ++ show n ++ ", " ++ call ++ ")"

mkArg :: EType -> Int -> String
mkArg t i = "mhs_to_" ++ cTypeName t ++ "(s, " ++ show i ++ ")"

mkParam :: EType -> Int -> String
mkParam t i = cfTypeName t ++ " a" ++ show i

mkArgNode :: EType -> Int -> String
mkArgNode t i = "NODEPTR arg" ++ show i ++ " = mfs_from_" ++ cTypeName' t ++ "(s, 1, a" ++ show i ++ ");"

mkApNode :: Int -> Int -> String
mkApNode k 0 = "NODEPTR p0 = new_app(xffe_table[" ++ show k ++ "], arg0);"
mkApNode _ i = "NODEPTR p" ++ show i ++ " = new_app(p" ++ show (i - 1) ++ ", arg" ++ show i ++ ");"

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

cfTypeName :: EType -> String
cfTypeName (EVar i) | Just c <- lookup (unIdent i) cfTypes = c
cfTypeName (ETuple []) = "void"
cfTypeName t = errorMessage (getSLoc t) $ "Not a valid foreign C type: " ++ showEType t

-- TODO: the foreign expr type are not fully qualified, e.g. Int is not Primitives.Int
cfTypes :: [(String, String)]
cfTypes =
  [ ("Int", "int")
  , ("Bool", "bool")
  ]

-- TODO: use cTypeName once foreign expr type are fully qualified
cTypeName' :: EType -> String
cTypeName' (EVar i) = unIdent i
cTypeName' (ETuple []) = "Unit"
cTypeName' _ = error "TODO!"

cTypeName :: EType -> String
cTypeName (EApp (EVar ptr) _t) | ptr == identPtr = "Ptr"
                               | ptr == identFunPtr = "FunPtr"
cTypeName (EVar i) | Just c <- lookup (unIdent i) cTypes = c
cTypeName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cTypes :: [(String, String)]
cTypes =
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
