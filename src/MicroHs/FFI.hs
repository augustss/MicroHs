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
      funcs' = map head $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) funcs
  in
    if not (null wrappers) || not (null dynamics) || not (null addrs) then error "Unimplemented FFI feature" else
    unlines (map (\ fn -> "#include \"" ++ fn ++ "\"") includes) ++
    unlines (map (uncurry mkStatic) funcs') ++
    "static struct ffi_entry table[] = {\n" ++
    unlines (map (mkEntry . fst) funcs') ++
    "{ 0,0 }\n};\n" ++
    "struct ffi_entry *xffi_table = table;\n" ++
    "\n"

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

mkEntry :: String -> String
mkEntry f = "{ \"" ++ f ++ "\", mhs_" ++ f ++ "},"

iIO :: Ident
iIO = mkIdent "Primitives.IO"

iUnit :: Ident
iUnit = mkIdent "Primitives.()"

mkStatic :: String -> EType -> String
mkStatic fn t =
  let !(as, ior) = getArrows t in
  case getApp iIO ior of
    Nothing -> errorMessage (getSLoc t) $ "foreign return type must be IO"
    Just r ->
      let
          n = length as
          call = fn ++ "(" ++ intercalate ", " (zipWith mkArg as [0..]) ++ ")"
          fcall =
            if isUnit r then
              call ++ "; mhs_from_Unit(s, " ++ show n ++ ")"
            else
              mkRet r ++ "(s, " ++ show n ++ ", " ++ call ++ ")"
      in  "void mhs_" ++ fn ++ "(int s) { " ++ fcall ++ "; }"

getApp :: Ident -> EType -> Maybe EType
getApp i (EApp (EVar i') t) | i == i' = Just t
getApp _ _ = Nothing

isUnit :: EType -> Bool
isUnit (EVar unit) = unit == iUnit
isUnit _ = False

mkRet :: EType -> String
mkRet t = "mhs_from_" ++ cTypeName t

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
