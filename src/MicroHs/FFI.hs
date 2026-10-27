module MicroHs.FFI(makeFFI) where
import qualified Prelude(); import MHSPrelude
import Data.Char
import Data.List
import MicroHs.Desugar(LDef, JSKind(..), jsKind)
import MicroHs.Error(mhsError)
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.Names
--import Debug.Trace

-- The export table has (internal-name, external-name, external-type)
-- Returns the C code and the header for the exports.
makeFFI :: Flags -> [(Ident, Ident, CType, IsJavascript)] -> [IdentModule] -> [[LDef]] -> (String, String)
makeFFI _ forExps exclude dss =
  let ffiImports = nubBy eq [ (ie, n, t, mn) | ds <- dss, (_, d) <- ds, Lit (LForImp mn ie n (CType t)) <- [get d] ]
                 where get (App _ a) = a   -- if there is no IO type, we have (App primPerform (LForImp ...))
                       get a = a
                       eq (_, n, _, _) (_, n', _, _) = n == n'
      wrappers = [ t | (ImpWrapper, _, t, _) <- ffiImports]
      dynamics = [ t | (ImpDynamic, _, t, _) <- ffiImports]
      imps     = filter ((`notElem` exclude) . impModule) $ filter ((`notElem` runtimeFFI) . impName) ffiImports
      includes = nub [ inc | (ImpStatic iincs _ _, _, _, _) <- imps, inc <- iincs ]
      isJS (ImpJS _ _, _, _, _) = True
      isJS _ = False
      -- JavaScript imports are only compiled for emscripten, so other targets can still compile the code.
      jsGuard imp str = if isJS imp then "#if defined(__EMSCRIPTEN__)\n" ++ str ++ "\n#endif" else str
      jsincs   = if any isJS ffiImports then ["#if defined(__EMSCRIPTEN__)", "#include \"emscripten.h\"", "#endif"] else []
      mkSig (_, i, CType t, js) = let (as, ior) = getArrows t in mkExportSig js i as ior ++ ";"
      header = unlines
        ["#include <stdint.h>",
         "#if defined(__cplusplus)",
         "extern \"C\" {",
         "#endif",
         "void mhs_init(void);",
         intercalate "\n" $ map mkSig forExps,
         "#if defined(__cplusplus)",
         "}",
         "#endif"
        ]
  in
    if not (null wrappers) || not (null dynamics) then mhsError "Unimplemented FFI feature" else
    (unlines $
      jsincs ++
      map (\ fn -> "#include \"" ++ fn ++ "\"") includes ++
      (if any (\ (_, _, _, js) -> js) forExps then
         ["#if defined(__EMSCRIPTEN__)",
          "#include \"emscripten.h\"",
          "#else",
          "#define EMSCRIPTEN_KEEPALIVE",
          "#endif"]
       else []) ++
      map (\ imp -> jsGuard imp (mkHdr imp)) imps ++
      ["static const struct ffi_entry imp_table[] = {"] ++
      map (\ imp -> jsGuard imp (mkEntry imp)) imps ++
      ["{ 0,0 }",
       "};",
       "const struct ffi_entry *xffi_table = imp_table;"
      ] ++
      ["static struct ffe_entry exp_table[] = {"] ++
      map mkExport forExps ++
      ["  { 0,0 }",
       "};",
       "struct ffe_entry *xffe_table = exp_table;",
       "\n"
      ] ++ zipWith mkExportWrapper [0..] forExps
    , header)

mkExportSig :: IsJavascript -> Ident -> [EType] -> EType -> String
mkExportSig js n as ior =
  let outT = expTypeName js $ checkIO ior
      ins = zipWith (\ i a -> expTypeName js a ++ " _x" ++ show i) [1::Int ..] as
   in outT ++ " " ++ unIdent n ++ "(" ++ intercalate ", " ins ++ ")"

mkExport :: (Ident, Ident, CType, IsJavascript) -> String
mkExport (i, _, _, _) = "  { \"" ++ unIdent i ++ "\", 0 },"

mkExportWrapper :: Int -> (Ident, Ident, CType, IsJavascript) -> String
mkExportWrapper no (_, n, CType t, js) = unlines $
  let (as, ior) = getArrows t
      r = checkIO ior
      outT = expTypeName js r
      arg k a = "  mhs_from_" ++ expTypeHsName js a ++ "(ffe_alloc(), 0, _x" ++ show k ++ "); ffe_apply();"
      eval = if eqEType r ior then "ffe_eval()" else "ffe_exec()"
  in  [(if js then "EMSCRIPTEN_KEEPALIVE " else "") ++ mkExportSig js n as ior ++ " {",
       "  gc_check(" ++ show (2 * length as + 4) ++ ");",
       "  ffe_push(xffe_table[" ++ show no ++ "].ffe_value);" ]
      ++ zipWith arg [1::Int ..] as ++
      if isUnit r then
        [ "  (void)" ++ eval ++ ";",
          "  ffe_pop();",
          "}"
        ]
       else
        [ "  " ++ outT ++ " _res = mhs_to_" ++ expTypeHsName js r ++ "(" ++ eval ++ ", -1);",
          "  ffe_pop();",
          "  return _res;",
          "}"
        ]

impName :: (ImpEnt, String, EType, IdentModule) -> String
impName (_, s, _, _) = s

impModule :: (ImpEnt, String, EType, IdentModule) -> IdentModule
impModule (_, _, _, m) = m

mkEntry :: (ImpEnt, String, EType, IdentModule) -> String
mkEntry (ImpStatic _ IFunc  _, f, t, _) = "{ \"" ++ f ++ "\", " ++ show (arity t) ++ ", mhs_" ++ f ++ "},"
mkEntry (ImpStatic _ IPtr   _, f, _, _) = "{ \"&" ++ f ++ "\", 0, mhs_addr_" ++ f ++ "},"
mkEntry (ImpStatic _ IValue _, f, _, _) = "{ \"" ++ f ++ "\", 0, mhs_" ++ f ++ "},"
mkEntry (ImpJS _ _,            f, t, _) = "{ \"" ++ f ++ "\", " ++ show (arity t) ++ ", mhs_" ++ f ++ "},"
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

mkHdr :: (ImpEnt, String, EType, IdentModule) -> String
mkHdr (ImpStatic _ IPtr fn, f, iot, _) =
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
mkHdr (ImpStatic _ IFunc fn, f, t, _) =
  let (as, ior) = getArrows t
      r = checkIO ior
      len = length as
      call = fn ++ "(" ++ intercalate ", " (zipWith mkArg as [0..]) ++ ")"
      fcall =
        if isUnit r then
          call ++ "; return mhs_from_Unit(s, " ++ show len ++ ")"
        else
          "return " ++ mkRet r len call
  in  mkMhsFun f fcall
mkHdr (ImpStatic _ IValue val, f, t, _) =
  let (as, ior) = getArrows t
      r = checkIO ior
      len = length as
      call = expand val
      expand [] = []
      expand ('$':c:cs) | isDigit c =
        let n = digitToInt c - 1
        in mkArg (as !! n) n ++ expand cs
      expand (c:cs) = c : expand cs
      fcall =
        if isUnit r then
          call ++ "; return mhs_from_Unit(s, " ++ show len ++ ")"
        else
          "return " ++ mkRet r len call
  in  mkMhsFun f fcall
mkHdr (ImpJS _ s, f, ty, _) | s == "wrapper" || s == "wrapper sync" =
  -- foreign import javascript "wrapper [sync]" mk :: (JSVal -> ... -> IO r) -> IO JSVal
  -- Creates a JavaScript function that calls the Haskell function.
  let (as, ior) = getArrows (dropForallContext ty)
      bad msg = errorMessage (getSLoc ty) $ "foreign import javascript \"" ++ s ++ "\": " ++ msg
      fty = case as of
              [t] -> t
              _   -> bad "expected one function argument"
      (fas, fior) = getArrows (dropForallContext fty)
      fr = checkIO fior
      ret = case jsKind fr of
              KUnit  -> "0"
              KJSVal -> "1"
              _      -> bad "the function must return IO () or IO JSVal"
      sync = if s == "wrapper sync" then "1" else "0"
      n = length fas
      call = "EM_ASM_INT({ return Module.mhsjs.mkCallback($0, " ++ show n ++ ", " ++ sync ++ ", " ++ ret ++ ") }, mhs_to_HsStablePtr(s, 0))"
  in  if any ((/= KJSVal) . jsKind) fas then bad "the function arguments must be JSVal" else
      if jsKind (checkIO ior) /= KJSVal then bad "the result must be IO JSVal" else
      mkMhsFun f ("return mhs_from_JSVal(s, 1, " ++ call ++ ")")
mkHdr (ImpJS sf s, f, ty, _) =
  -- The JavaScript code becomes an EM_JS function, mhs_js_<f>, whose parameters are
  -- $1, $2, ... (so the code can use them directly), emitted into the output by
  -- emscripten like any other JavaScript in the program.  It is passed to _EM_JS
  -- (what EM_JS expands to) as a C string, so the code need not consist of C tokens.
  -- The code is an expression or statements (with 'return'), see jsIsStatements.
  -- With 'unsafe' a JavaScript exception is fatal, with 'safe' it is turned into
  -- a Haskell JSException.  With 'interruptible' the code runs as an async function
  -- (so it can use await) and the C function returns its Promise as a JSVal; the
  -- desugarer wraps the import so that the Haskell thread waits for the Promise
  -- (MicroHs.Desugar.jsAwait, the IO.jsawait primitive), which converts the result.
  let (as, ior) = getArrows (dropForallContext ty)
      rt = checkIO ior
      rk = jsKind rt
      n = length as
      ixs = [0 .. n-1]
      param i = '$' : show (i + 1)
      jsName = "mhs_js_" ++ f
      ctypeOf t =
        case jsKind t of
          KJSVal  -> "int"
          KBool   -> "int"
          KInt    -> "intptr_t"
          KWord   -> "uintptr_t"
          KDouble -> "double"
          KFloat  -> "double"
          KPtr    -> "void *"
          KUnit   -> "void"
      params = "(" ++ intercalate ", " [ ctypeOf t ++ " " ++ param i | (t, i) <- zip as ixs ] ++ ")"
      -- Convert the arguments in place.
      conv t i =
        case jsKind t of
          KJSVal -> param i ++ " = Module.mhsjs.getJSVal(" ++ param i ++ "); "
          KBool  -> param i ++ " = !!" ++ param i ++ "; "
          _      -> ""
      -- The code can use mhsjs, the JSVal table.
      convs = "var mhsjs = Module.mhsjs; " ++ concat (zipWith conv as ixs)
      -- The code as a function of no arguments; the newline ends a trailing // comment.
      async = if sf == Interruptible then "async " else ""
      thunk = if jsIsStatements s then "(" ++ async ++ "() => {" ++ s ++ "\n})" else "(" ++ async ++ "() => (" ++ s ++ "\n))"
      value = thunk ++ "()"
      res =
        case rk of
          KUnit   -> value ++ "; return;"
          KJSVal  -> "return Module.mhsjs.newJSVal(" ++ value ++ ");"
          KBool   -> "return " ++ value ++ " ? 1 : 0;"
          _       -> "return " ++ value ++ ";"
      body
        -- The Promise of the async function; an exception in the argument conversion
        -- (e.g. a freed JSVal) becomes a rejected Promise, i.e. a JSException.
        | sf == Interruptible =
            "try { " ++ convs ++ "return Module.mhsjs.newJSVal(" ++ value ++ "); }" ++
            " catch (e) { return Module.mhsjs.newJSVal(Promise.reject(e)); }"
        -- With unsafe a JavaScript exception is fatal: it unwinds the C stack, so the runtime
        -- is marked as stopped (Module.mhsjs.fatal) before the exception propagates.
        | sf == Unsafe =
            "try { " ++ convs ++ res ++ " } catch (e) { throw Module.mhsjs.fatal(e); }"
        -- With safe any JavaScript exception (also in the argument conversion, e.g. a
        -- freed JSVal) is saved and raised as a JSException by mhs_js_check_error.
        | otherwise =
            "try { " ++ convs ++ res ++ " } catch (e) { Module.mhsjs.error = e; Module.mhsjs.hasError = true; return 0; }"
      cret = if sf == Interruptible then "int" else ctypeOf rt
      emjs = "_EM_JS(" ++ cret ++ ", " ++ jsName ++ ", " ++ jsName ++ ", " ++ params ++ ", " ++ cString ("{ " ++ body ++ " }") ++ ")\n"
      cargs = intercalate ", " (zipWith mkJSArg as ixs)
      call = jsName ++ "(" ++ cargs ++ ")"
      check = if sf == Unsafe then "" else "mhs_js_check_error(); "
      ret r = case rk of
                KUnit -> "return mhs_from_Unit(s, " ++ show n ++ ")"
                _     -> "return mhs_from_" ++ jsTypeName rt ++ "(s, " ++ show n ++ ", " ++ r ++ ")"
      fbody =
        case rk of
          KUnit -> call ++ "; " ++ check ++ ret ""
          _     -> cret ++ " r = " ++ call ++ "; " ++ check ++ ret "r"
  in  if sf == Interruptible then
        emjs ++ mkMhsFun f ("int r = " ++ call ++ "; return mhs_from_JSVal(s, " ++ show n ++ ", r)")
      else
        emjs ++ mkMhsFun f fbody
mkHdr _ = undefined

-- Is the JavaScript code statements (a function body) rather than an expression?
-- Yes if, outside brackets, strings and comments, it has a ';' or the word 'return',
-- or it starts with a word that only starts a statement.  This is the same choice
-- as trying the code as an expression first and as statements if that is a syntax
-- error, which is what the code did when it was compiled at run time.
jsIsStatements :: String -> Bool
jsIsStatements src =
  case tokens (0 :: Int) src of
    ts -> any topStmt ts || (case [ w | (0, Word w) <- take 1 ts ] of
                               [w] -> w `elem` stmtWords
                               _   -> False)
  where
    topStmt (0, Semi) = True
    topStmt (0, Word "return") = True
    topStmt _ = False
    stmtWords = ["if", "for", "while", "do", "switch", "try", "throw", "let", "const", "var",
                 "function", "class", "return", "break", "continue"]
    tokens _ [] = []
    tokens d ('/':'/':cs) = tokens d (dropWhile (/= '\n') cs)
    tokens d ('/':'*':cs) = tokens d (skipComment cs)
    tokens d (c:cs)
      | c `elem` "\"'`" = tokens d (skipString c cs)
      | c `elem` "([{"   = tokens (d + 1) cs
      | c `elem` ")]}"   = tokens (max 0 (d - 1)) cs
      | c == ';'         = (d, Semi) : tokens d cs
      | isAlpha c || c == '_' || c == '$' =
          case span (\ x -> isAlphaNum x || x == '_' || x == '$') (c:cs) of
            (w, r) -> (d, Word w) : tokens d r
      | otherwise        = tokens d cs
    skipComment ('*':'/':cs) = cs
    skipComment (_:cs) = skipComment cs
    skipComment [] = []
    skipString q ('\\':_:cs) = skipString q cs
    skipString q (c:cs) | c == q = cs
                        | otherwise = skipString q cs
    skipString _ [] = []

data JSToken = Semi | Word String

arity :: EType -> Int
arity = length . fst . getArrows . dropForallContext

-- Use to construct 'foreign import/export ccall' wrapper.
cTypeHsName :: HasCallStack => EType -> String
cTypeHsName (EApp (EVar ptr) _t) | ptr == identPtr = "Ptr"
                                 | ptr == identFunPtr = "FunPtr"
cTypeHsName (EVar i) | Just c <- lookup (unIdent i) cHsTypes = c
cTypeHsName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cHsTypes :: [(String, String)]
cHsTypes =
  [ ("Primitives.Float",  "Float")
  , ("Primitives.Double", "Double")
  , ("Primitives.Int",    "Int")
  , ("Primitives.Int64",  "Int64")
  , ("Primitives.Word",   "Word")
  , ("Primitives.Word64", "Word64")
  , ("()",                "Unit")
  , ("System.IO.Handle",  "Ptr")
  ]

-- Foreign export type names; a javascript export also allows Bool.
expTypeName :: IsJavascript -> EType -> String
expTypeName True (EVar i) | unIdent i == "Data.Bool_Type.Bool" = "int"
expTypeName _ t = cTypeName t

expTypeHsName :: IsJavascript -> EType -> String
expTypeHsName True (EVar i) | unIdent i == "Data.Bool_Type.Bool" = "Bool"
expTypeHsName _ t = cTypeHsName t

-- Use to construct 'foreign export ccall' signature.
cTypeName :: EType -> String
cTypeName (EApp (EVar ptr) _t) | ptr == identPtr = "void*"
cTypeName (EVar i) | Just c <- lookup (unIdent i) cTypes = c
cTypeName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cTypes :: [(String, String)]
cTypes =
  [ ("Primitives.Float",  "float")
  , ("Primitives.Double", "double")
  , ("Primitives.Int",    "intptr_t")   -- value_t
  , ("Primitives.Int64",  "int64_t")
  , ("Primitives.Word",   "uintptr_t")  -- uvalue_t
  , ("Primitives.Word64", "uint64_t")
  , ("()",                "void")
  , ("System.IO.Handle",  "void*")
  ]

-- The mhs_to_ function to use for a JavaScript argument.
jsTypeName :: EType -> String
jsTypeName t =
  case jsKind t of
    KJSVal  -> "JSVal"
    KBool   -> "Bool"
    KInt    -> "Int"
    KWord   -> "Word"
    KDouble -> "Double"
    KFloat  -> "Float"
    KPtr    -> "Ptr"
    KUnit   -> errorMessage (getSLoc t) "() is not a valid JavaScript argument type"

-- A C string literal.
cString :: String -> String
cString str = '"' : concatMap esc str ++ "\""
  where esc '"'  = "\\\""
        esc '\\' = "\\\\"
        esc c | c < ' ' || c == '\DEL' = '\\' : oct (fromEnum c)
              | otherwise = [c]
        oct n = [toEnum (fromEnum '0' + n `div` 64), toEnum (fromEnum '0' + (n `div` 8) `mod` 8), toEnum (fromEnum '0' + n `mod` 8)]

-- These are already in the runtime
runtimeFFI :: [String]
runtimeFFI = [
  "GETRAW", "GETTIMEMICRO", "GETBOOTTIMEMICRO", "acos", "add_FILE", "add_fd", "open", "add_utf8", "add_buf", "add_crlf",
  "asin", "atan", "atan2", "calloc", "closeb",
  "cos", "exp", "flushb", "fopen", "free", "getb", "getenv", "isattyb", "filesizeb", "islinux", "ismacos", "iswindows", "log", "malloc",
  "md5Array", "md5BFILE", "md5String", "memcpy", "memmove", "realloc", "strlen", "strcpy",
  "putb", "sin", "sqrt", "system", "tan", "tmpname", "ungetb", "remove",
  "acosf", "asinf", "atanf", "atan2f", "cosf", "expf", "logf", "sinf", "sqrtf", "tanf",
  "scalbn", "scalbnf", "pow", "powf",
  "js_debug", "js_eval_run", "js_eval_call", "js_set_haskellCallback", "js_free_jsval", "js_exn_string",
  "readb", "writeb",
  "peekPtr", "pokePtr", "pokeWord", "peekWord",
  "add_lz77_compressor", "add_lz77_decompressor",
  "add_lzma_compressor", "add_lzma_decompressor",
  "add_rle_compressor", "add_rle_decompressor",
  "add_base64_encoder", "add_base64_decoder",
  "add_bwt_compressor", "add_bwt_decompressor",
  "peek_uint8", "poke_uint8", "peek_uint16", "poke_uint16", "peek_uint32", "poke_uint32", "peek_uint64", "poke_uint64",
  "peek_int8", "poke_int8", "peek_int16", "poke_int16", "peek_int32", "poke_int32", "peek_int64", "poke_int64",
  "peek_char", "poke_char", "peek_schar", "poke_schar", "peek_uchar", "poke_uchar",
  "peek_ushort", "poke_ushort", "peek_short", "poke_short",
  "peek_uint", "poke_uint", "peek_int", "poke_int",
  "peek_ulong", "poke_ulong", "peek_long", "poke_long",
  "peek_ullong", "poke_ullong", "peek_llong", "poke_llong",
  "peek_size_t", "poke_size_t",
  "peek_flt32", "poke_flt32",
  "peek_flt64", "poke_flt64",
  "sizeof_char", "sizeof_short", "sizeof_int", "sizeof_long", "sizeof_llong", "sizeof_size_t",
  "opendir", "closedir", "readdir", "c_d_name", "chdir", "mkdir", "getcwd",
  "getcpu",
  "get_mem", "openb_rd_mem", "openb_wr_mem",
  "new_mpz", "mpz_abs", "mpz_add", "mpz_and", "mpz_cmp", "mpz_get_d", "mpz_get_f",
  "mpz_get_si", "mpz_init_set_si", "mpz_init_set_ui",
  "mpz_ior",
  "mpz_mul", "mpz_mul_2exp", "mpz_neg", "mpz_popcount", "mpz_sub", "mpz_fdiv_q_2exp",
  "mpz_tdiv_qr", "mpz_tstbit", "mpz_xor",
  "mpz_get_si64", "mpz_init_set_si64", "mpz_init_set_ui64",
  "mpz_log2",
  "want_gmp",
  "want_imath",
  "gettimeofday",
  "EOK", "E2BIG", "EACCES", "EADDRINUSE", "EADDRNOTAVAIL", "EADV", "EAFNOSUPPORT", "EAGAIN",
  "EALREADY", "EBADF", "EBADMSG", "EBADRPC", "EBUSY", "ECHILD", "ECOMM", "ECONNABORTED",
  "ECONNREFUSED", "ECONNRESET", "EDEADLK", "EDESTADDRREQ", "EDIRTY", "EDOM", "EDQUOT",
  "EEXIST", "EFAULT", "EFBIG", "EFTYPE", "EHOSTDOWN", "EHOSTUNREACH", "EIDRM", "EILSEQ",
  "EINPROGRESS", "EINTR", "EINVAL", "EIO", "EISCONN", "EISDIR", "ELOOP", "EMFILE", "EMLINK",
  "EMSGSIZE", "EMULTIHOP", "ENAMETOOLONG", "ENETDOWN", "ENETRESET", "ENETUNREACH",
  "ENFILE", "ENOBUFS", "ENODATA", "ENODEV", "ENOENT", "ENOEXEC", "ENOLCK", "ENOLINK",
  "ENOMEM", "ENOMSG", "ENONET", "ENOPROTOOPT", "ENOSPC", "ENOSR", "ENOSTR", "ENOSYS",
  "ENOTBLK", "ENOTCONN", "ENOTDIR", "ENOTEMPTY", "ENOTSOCK", "ENOTSUP", "ENOTTY", "ENXIO",
  "EOPNOTSUPP", "EPERM", "EPFNOSUPPORT", "EPIPE", "EPROCLIM", "EPROCUNAVAIL",
  "EPROGMISMATCH", "EPROGUNAVAIL", "EPROTO", "EPROTONOSUPPORT", "EPROTOTYPE",
  "ERANGE", "EREMCHG", "EREMOTE", "EROFS", "ERPCMISMATCH", "ERREMOTE", "ESHUTDOWN",
  "ESOCKTNOSUPPORT", "ESPIPE", "ESRCH", "ESRMNT", "ESTALE", "ETIME", "ETIMEDOUT",
  "ETOOMANYREFS", "ETXTBSY", "EUSERS", "EWOULDBLOCK", "EXDEV",
  "errno",
  "strerror_r",
  "environ",
  "get_executable_path",
  "setenv", "unsetenv",
  "set_permissions", "get_permissions", "get_file_size",
  "F_SETFL", "O_NONBLOCK", "SOL_SOCKET", "SO_DEBUG", "SO_ERROR", "SO_REUSEADDR", "SO_TYPE",
  "accept", "bind", "close", "connect", "fcntl", "getsockopt", "listen", "recv", "send", "setsockopt", "socket"
  ]

{-
-- lib/ modules that use foreign import
libImports :: [String]
libImports = [
  "Data.Integer_Type",
  "Data.Integer.Internal",
  "System.Process",
  "System.Environment",
  "System.Compress.ByteString",
  "System.Compress",
  "System.IO.TimeMilli",
  "System.IO.Open",
  "System.IO.Transducers",
  "System.IO.MD5",
  "System.IO.Base",
  "System.IO.StringHandle",
  "System.IO.Internal",
  "System.IO.Serialize",
  "System.CPUTime",
  "System.Directory",
  "System.Cmd",
  "Data.ByteString",
  "Data.Double",
  "Data.Float",
  "Foreign.Marshal.Utils",
  "Foreign.Marshal.Alloc",
  "Foreign.Storable",
  "Foreign.C.Error",
  "Primitives"
  ]
-}
