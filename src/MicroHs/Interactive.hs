module MicroHs.Interactive(mainInteractive) where
import qualified Prelude(); import MHSPrelude
import Data.List
import Data.Maybe
import Data.Version
import Control.Exception
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.Desugar(LDef)
import MicroHs.Expr(EType, showEType)
import MicroHs.Flags
import MicroHs.Ident(mkIdent, Ident, unIdent, isIdentChar)
import MicroHs.List
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.SymTab(Entry(..), stEmpty, stKeysGlbU)
import MicroHs.Translate
import MicroHs.TypeCheck(ValueExport(..), TypeExport(..), TModule(..), Symbols)
import Unsafe.Coerce
import System.Console.SimpleReadline
import Paths_MicroHs(version)
import System.IO

data IState = IState {
  isLines   :: String,
  isFlags   :: Flags,
  isCache   :: Cache,
  isSymbols :: Symbols,
  isStats   :: Bool
  }

type I a = StateIO IState a

mainInteractive :: Flags -> IO ()
mainInteractive flags = do
  putStrLn $ "Welcome to interactive MicroHs, version " ++ showVersion version
  when wantGMP $ putStrLn "Using GMP"
  let flags' = flags{ loading = True }
  cash <- getCached flags'
  _ <- runStateIO start $ IState preamble flags' cash noSymbols False
  return ()

noSymbols :: Symbols
noSymbols = (stEmpty, stEmpty)

preamble :: String
preamble = "module " ++ interactiveName ++ " where\n\
           \import System.IO.PrintOrRun\n\
           \default Num (Integer, Double)\n\
           \default IsString (String)\n\
           \default Show (())\n"

start :: I ()
start = do
  reload
  is <- get
  liftIO $ maybeSaveCache (isFlags is) (isCache is)
  liftIO $ putStrLn "Type ':quit' to quit, ':help' for help"
  unless compiledWithMhs $ do
    --liftIO $ putStrLn "WARNING: Not compiled with mhs, so limited functionality."
    error "The interactive system currently only works with mhs"
  repl

repl :: I ()
repl = do
  mdls <- gets (cachedModuleNames . isCache)
  syms <- gets isSymbols
  stdinFlag <- gets (useStdin . isFlags)
  ms <- liftIO $
    if stdinFlag then do
      putStr "> "
      hFlush stdout
      es <- try getLine
      case es of
        Left  (_::SomeException) -> return Nothing
        Right s                  -> return (Just s)
    else do
      ms <- getInputLineHistComp (return . complete mdls syms) ".mhsi" "> "
      return (ms <|> Just "")  -- ignore ^D
  let bye = liftIO $ putStrLn "Bye"
  case ms of
    Nothing -> bye
    Just s ->
      case s of
        [] -> repl
        ':':r -> do
          c <- command r
          if c then repl else bye
        _ -> do
          oneline s
          repl

command :: String -> I Bool
command s =
  case words s of
    [] -> return True
    c : ws ->
      case filter (isPrefixOf c . (!!0) . words . fst) commands of
        [] -> do
          liftIO $ putStrLn "Unrecognized command"
          return True
        [(_, cmd)] ->
          cmd (unwords ws)
        xs -> do
          liftIO $ putStrLn $ "Ambiguous command: " ++ unwords (map fst xs)
          return True

commands :: [(String, String -> I Bool)]
commands =
  [ ("quit      quit MicroHs",
     const $ return False
    )
  , ("clear     clear all definitions", const $ do
      updateLines (const preamble)
      modify $ \ is -> is{ isCache = emptyCache, isSymbols = noSymbols }
      return True
    )
  , ("reload    reload modules", const $ do
      flgs <- gets isFlags
      cash <- gets isCache
      cash' <- liftIO $ validateCache flgs cash
      modify $ \ is -> is{ isCache = cash' }
      reload
      return True
    )
  , ("delete d  delete definition(s) d", \ line -> do
      updateLines (unlines . filter (not . isPrefixOf line) . lines)
      return True
    )
  , ("type e    show type of e", \ line -> do
      showType line
      return True
    )
  , ("kind t    show kind of t", \ line -> do
      showKind line
      return True
    )
  , ("main args run main with arguments", \ line -> do
      runMain line
      return True
    )
  , ("defs      show current definitions", const $ do
      showDefs
      return True
    )
  , ("help      this text", const $ do
      liftIO $ putStrLn $ helpText ++ unlines (map ((':' :) . fst) commands)
      return True
    )
  , ("set f     (un)set flag", \ line -> do
      setFlags line
      return True
    )
  ]

setFlags :: String -> I ()
setFlags "" = do
  stats <- gets isStats
  liftIO $ putStrLn "Current flags: (use + to set and - to unset)"
  liftIO $ putStrLn $ "  " ++ (if stats then "+" else "-") ++ "s"
setFlags "+s" = do
  modify $ \ is -> is{ isStats = True }
setFlags "-s" = do
  modify $ \ is -> is{ isStats = False }
setFlags _ =
  liftIO $ putStrLn "Unknown flag.  Known flags: +s, -s"

reload :: I ()
reload = do
  ls <- gets isLines
  rld <- tryCompile ls   -- reload modules right away
  case rld of
    Left msg -> liftIO $ err msg
    Right _  -> return ()

helpText :: String
helpText = "\
  \Commands:\n\
  \expr       evaluate expression\n\
  \defn       add top level definition\n\
  \"

updateLines :: (String -> String) -> I ()
updateLines f = modify $ \ is -> is{ isLines = f (isLines is) }

updateCache :: (Cache -> Cache) -> I ()
updateCache f = modify $ \ is -> is{ isCache = f (isCache is) }

setSyms :: Symbols -> I ()
setSyms syms = modify $ \ is -> is{ isSymbols = syms }

interactiveName :: String
interactiveName = "Interactive"

interactiveId :: Ident
interactiveId = mkIdent interactiveName

itName :: String
itName = "_it"

itTypeName :: String
itTypeName = "Type_it"

itIOName :: String
itIOName = "_itIO"

mkIt :: String -> String
mkIt l =
  itName ++ " = " ++ l ++ "\n"

mkItIO :: Bool -> String -> String
mkItIO stats l =
  let prt = if stats then "_printOrRunStats" else "_printOrRun"
  in  mkIt l ++
      itIOName ++ " = " ++ prt ++ " " ++ itName ++ "\n"

mkTypeIt :: String -> String
mkTypeIt l =
  "type " ++ itTypeName ++ " = " ++ l ++ "\n"

err :: SomeException -> IO ()
err e = err' $ displayException e

err' :: String -> IO ()
err' s = putStrLn $ "*** Exception: " ++ s

oneline :: String -> I ()
oneline line = do
  ls <- gets isLines
  stats <- gets isStats
  let lls = ls ++ line ++ "\n"
      def = do
        defTest <- tryCompile lls
        case defTest of
          Right _ -> updateLines (const lls)
          Left  e -> liftIO $ err e
      expr = do
        exprTest <- tryCompile (ls ++ "\n" ++ mkItIO stats line)
        case exprTest of
          Right m -> evalExpr m
          Left  e -> liftIO $ err e
  -- First try to parse as a definition,
  tryParse pTopModule lls def $ \ _ ->
    -- if that fails, parse as an expression.
    tryParse pExprTop line expr $
      liftIO . err'

tryParse :: forall a . Show a => P a -> String -> I () -> (String -> I ()) -> I ()
tryParse p s ok bad =
  case parse p "" s of
    Right _ -> ok
    Left  e -> bad e

tryCompile :: String -> I (Either SomeException [LDef])
tryCompile file = do
  updateCache (deleteFromCache interactiveId)
  flgs <- gets isFlags
  cash <- gets isCache
  liftIO $ writeFile (interactiveName ++ ".hs") file
  res <- liftIO $ try $ compileCacheTop flgs interactiveId cash
  case res of
    Left e -> return (Left e)
    Right ((_, m), syms, cash') -> do
      updateCache (const cash')
      setSyms syms
      return (Right m)

evalExpr :: [LDef] -> I ()
evalExpr cmdl = do
  let ares = translate (mkIdent (interactiveName ++ "." ++ itIOName), cmdl)
      res = unsafeCoerce ares :: IO ()
  mval <- liftIO $ try (seq res (return res))
  liftIO $
    case mval of
      Left  e -> err e
      Right val -> do
        mio <- try val
        case mio of
          Left  e -> err e
          Right _ -> return ()

showType :: String -> I ()
showType line = do
  ls <- gets isLines
  res <- tryCompile (ls ++ "\n" ++ mkIt line)
  case res of
    Right _ -> do
      cash <- gets isCache
      let t = getTypeInCache cash (mkIdent itName)
      liftIO $ putStrLn $ showEType t
    Left  e ->
      liftIO $ err e

showKind :: String -> I ()
showKind line = do
  ls <- gets isLines
  res <- tryCompile (ls ++ "\n" ++ mkTypeIt line)
  case res of
    Right _ -> do
      cash <- gets isCache
      let t = getKindInCache cash (mkIdent itTypeName)
      liftIO $ putStrLn $ showEType t
    Left  e ->
      liftIO $ err e

runMain :: String -> I ()
runMain line = oneline $ "_withArgs " ++ show (words line) ++ " main"

showDefs :: I ()
showDefs = do
  ls <- gets isLines
  liftIO $ putStrLn ls

getCModule :: Cache -> TModule [LDef]
getCModule cash =
  case lookupCache interactiveId cash of
    Nothing -> undefined   -- this cannot happen
    Just cm -> cm

getTypeInCache :: Cache -> Ident -> EType
getTypeInCache cash i =
  head $ [ t | ValueExport i' (Entry _ t) <- tValueExps (getCModule cash), i == i' ] ++ [undefined]

getKindInCache :: Cache -> Ident -> EType
getKindInCache cash i =
  head $ [ k | TypeExport i' (Entry _ k) _ <- tTypeExps (getCModule cash), i == i' ] ++ [undefined]

-- This could be smarter:
--  ":a"        should complete with commands
--  "import A"  should complete with modules
--  operator completion
--  completion with qualified names

complete :: [Ident] -> Symbols -> (String, String) -> [String]
complete mdls (tys, vals) (rpre, _post) =
  let pre = reverse $ takeWhile isId rpre
      isId c = isIdentChar c || c == '.'
      allSyms = map unIdent $ stKeysGlbU tys ++ stKeysGlbU vals ++ mdls
      allStrs = allSyms ++ keywords
      real = notElem '$'
  in  case filter real $ mapMaybe (stripPrefix pre) allStrs of
        []  -> []
        [s] -> [s ++ " "]
        ss  ->
          case findCommonPrefix ss of
            [] -> ss
            p  -> [p]
