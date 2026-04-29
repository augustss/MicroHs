module MicroHs.Interactive(mainInteractive, mainEvalArg) where
import qualified Prelude(); import MHSPrelude
import Data.Char
import Data.List
import Data.Maybe
import Data.Version
import Control.Exception
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.Desugar(LDef)
import MicroHs.Exp(Exp(Var))
import MicroHs.Expr(showEType, EModule(..), EDef(..), Expr(EVar))
import MicroHs.Flags
import MicroHs.Ident(mkIdent, Ident, unIdent, isIdentChar, SLoc(..), slocIdent, isNoSLoc)
import qualified MicroHs.IdentMap as M
import MicroHs.List
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.SymTab(Entry(..), stEmpty, stKeysGlbU, stLookup)
import MicroHs.Translate
import MicroHs.TCMonad(TCState(..), tcStateToXTCState)
import MicroHs.TypeCheck(TModule(..), Symbols)
import MicroHs.Version
import Unsafe.Coerce
import System.Console.SimpleReadline
import System.Cmd
import System.Environment
import System.FilePath
import System.IO
import Text.Printf
import Text.Read(readMaybe)
--import System.IO.TimeMilli

defaultEditor :: String
defaultEditor = "vim +%d %s"

data IState = IState {
  isLines   :: String,
  isFlags   :: Flags,
  isCache   :: Cache,
  isSymbols :: Symbols,
  isStats   :: Bool,
  isCComp   :: (Int, TCState, TranslateMap),
  isHistory :: FilePath,
  isErrLine :: Int,
  isErrFile :: FilePath
  }

-- To speed up interactive use, the state of the symbol table after
-- processing all imports is cached in isCComp together with the
-- mapping from all imported names to their actual values.
-- Every time the imports change, these tables are recomputed.

type I a = StateIO IState a

mainInteractive :: Flags -> [String] -> IO ()
mainInteractive flags mdls = do
  putStrLn $ "Welcome to interactive MicroHs, version " ++ showVersion version
  when wantGMP $ putStrLn "Using GMP"
  mhome <- lookupEnv "HOME"
  let flags' = flags{ loading = True }
      hist = maybe mhsi (</> mhsi) mhome
      mhsi = ".mhsi"
  _ <- runStateIO startInteractive =<< startIState flags' mdls hist
  return ()

mainEvalArg :: Flags -> String -> [String] -> IO ()
mainEvalArg flags arg mdls = do
  let eval = do
        reload
        oneline arg
  _ <- runStateIO eval =<< startIState flags mdls (error "hist")
  return ()

startIState :: Flags -> [String] -> String -> IO IState
startIState flags mdls hist = do
  unless compiledWithMhs $ do
    --putStrLnI "WARNING: Not compiled with mhs, so limited functionality."
    mhsError "The interactive system currently only works with mhs"
  cash <- getCached flags
  let startMdl = preamble ++ unlines (map ("import " ++) mdls)
  return $ IState startMdl flags cash noSymbols False (-1, error "tcstate", M.empty) hist 1 ""

noSymbols :: Symbols
noSymbols = (stEmpty, stEmpty)

preamble :: String
preamble = "module " ++ interactiveName ++ " where\n\
           \import qualified System.IO.PrintOrRun\n\
           \default Num (Integer, Double)\n\
           \default IsString (String)\n\
           \default Show (())\n"

putStrLnI :: String -> I ()
putStrLnI = liftIO . putStrLn

startInteractive :: I ()
startInteractive = do
  reload
  is <- get
  liftIO $ maybeSaveCache (isFlags is) (isCache is)
  putStrLnI "Type ':quit' to quit, ':help' for help"
  repl

repl :: I ()
repl = do
  mdls <- gets (cachedModuleNames . isCache)
  syms <- gets isSymbols
  stdinFlag <- gets (useStdin . isFlags)
  hist <- gets isHistory
  ms <- liftIO $
    if stdinFlag then do
      putStr "> "
      hFlush stdout
      es <- try getLine
      case es of
        Left  (_::SomeException) -> return Nothing
        Right s                  -> return (Just s)
    else do
      ms <- getInputLineHistComp (return . complete mdls syms) hist "> "
      return (ms <|> Just "")  -- ignore ^D
  let bye = putStrLnI "Bye"
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
  let fw = (!!0) . words . fst in
  case words s of
    [] -> return True
    c : ws ->
      case filter (isPrefixOf c . fw) commands of
        [] -> do
          putStrLnI "Unrecognized command"
          return True
        [(_, cmd)] ->
          cmd (unwords ws)
        xs -> do
          putStrLnI $ "Ambiguous command: " ++ unwords (map fw xs)
          return True

commands :: [(String, String -> I Bool)]
commands =
  [ ("quit        quit MicroHs",
     const $ return False
    )
  , ("clear       clear all definitions", const $ do
      updateLines (const preamble)
      modify $ \ is -> is{ isCache = emptyCache, isSymbols = noSymbols }
      return True
    )
  , ("reload      reload modules", const $ do
      reload
      return True
    )
  , ("delete PRE  delete definition(s) with prefix PRE", \ line -> do
      updateLines (unlines . filter (not . isPrefixOf line) . lines)
      return True
    )
  , ("type EXPR   show type of EXPR", \ line -> do
      showType line
      return True
    )
  , ("kind TYPE   show kind of TYPE", \ line -> do
      showKind line
      return True
    )
  , ("main ARGS   run main with arguments", \ line -> do
      runMain line
      return True
    )
  , ("defs        show current definitions", const $ do
      showDefs
      return True
    )
  , ("save [FILE] save current definitions", \ line -> do
      saveDefs line
      return True
    )
  , ("edit [FILE] edit file or last error location", \ line -> do
      edit line
      return True
    )
  , ("find NAME   find definition of NAME", \ line -> do
      finds line
      return True
    )
  , ("help        this text", help
    )
  , ("?           this text", help
    )
  , ("! CMD       run shell command", \ line -> do
      _ <- liftIO $ system line
      return True
    )
  , ("set [FLAG]  (un)set flag", \ line -> do
      setFlags line
      return True
    )
  ]

help :: String -> I Bool
help = const $ do
  putStrLnI $ helpText ++ unlines (map ((':' :) . fst) commands)
  return True

setFlags :: String -> I ()
setFlags "" = do
  stats <- gets isStats
  putStrLnI "Current flags: (use + to set and - to unset)"
  putStrLnI $ "  " ++ (if stats then "+" else "-") ++ "s"
setFlags "+s" = do
  modify $ \ is -> is{ isStats = True }
setFlags "-s" = do
  modify $ \ is -> is{ isStats = False }
setFlags _ =
  putStrLnI "Unknown flag.  Known flags: +s, -s"

reload :: I ()
reload = do
  cash <- gets isCache
  flags <- gets isFlags
  cash' <- liftIO $ validateCache flags cash
  modify $ \ is -> is{ isCache = cash', isCComp = (-1, undefined, undefined) }
  ls <- gets isLines
  rld <- tryCompile ls   -- reload modules right away
  case rld of
    Left msg -> err msg
    Right _  -> return ()

helpText :: String
helpText = "\
  \Commands (may be abbreviated):\n\
  \EXPR         evaluate expression\n\
  \DEFN         add top level definition\n\
  \"

updateLines :: (String -> String) -> I ()
updateLines f = modify $ \ is -> is{ isLines = f (isLines is) }

--updateCache :: (Cache -> Cache) -> I ()
--updateCache f = modify $ \ is -> is{ isCache = f (isCache is) }

--setSyms :: Symbols -> I ()
--setSyms syms = modify $ \ is -> is{ isSymbols = syms }

interactiveName :: String
interactiveName = "Interactive"

--interactiveId :: Ident
--interactiveId = mkIdent interactiveName

itName :: String
itName = "_it"

itTypeName :: String
itTypeName = "Type_it"

itIOName :: String
itIOName = "_itIO"

mkIt :: String -> String
mkIt l =
  itName ++ " = " ++ l ++ "\n"

mkItIO :: Flags -> Bool -> String -> String
mkItIO flgs stats l =
  let prt = fromMaybe
              (if stats then "System.IO.PrintOrRun._printOrRunStats" else "System.IO.PrintOrRun._printOrRun")
              (iPrint flgs)
  in  mkIt l ++
      itIOName ++ " = " ++ prt ++ " " ++ itName ++ "\n"

mkTypeIt :: String -> String
mkTypeIt l =
  "type " ++ itTypeName ++ " = " ++ l ++ "\n"

err :: SomeException -> I ()
err e = do
  let msg = displayException e
  case parseError msg of
    Just (f, l) -> modify $ \ is -> is{ isErrFile = f, isErrLine = l }
    Nothing -> return ()
  liftIO $ err' msg

-- Try to find a file and line number
parseError :: String -> Maybe (FilePath, Int)
parseError s =
  case words s of
    "error:" : ('"' : sfile) : "line" : sline : _ |
      Just file <- stripSuffix "\":" sfile,
      Just line <- stripSuffix "," sline >>= readMaybe -> Just (file, line)
    _ -> Nothing

err' :: String -> IO ()
err' s = putStrLn $ "*** Exception: " ++ s

oneline :: String -> I ()
oneline aline = do
  let line = dropWhile isSpace aline
  ls <- gets isLines
  stats <- gets isStats
  let lls = ls ++ line ++ "\n"
      def = do
        defTest <- tryCompile lls
        case defTest of
          Right _ -> do
            updateLines (const lls)
          Left  e -> err e
      expr = do
--        t1 <- liftIO getTimeMilli
        flgs <- gets isFlags
        exprTest <- tryCompile (ls ++ "\n" ++ mkItIO flgs stats line)
        case exprTest of
          Right (m, _) -> do
            evalExpr m
{-
            t2 <- liftIO getTimeMilli
            when (stats) $
              liftIO $ putStrLn $ "total " ++ show (t2 - t1) ++ "ms"
-}
          Left  e -> err e
  -- First try to parse as a definition,
  tryParse pTopModule lls def $ \ _ ->
    -- if that fails, parse as an expression.
    tryParse pExprTop line expr $
      liftIO . err'

tryParse :: forall a . -- Show a =>
            P a -> String -> I () -> (String -> I ()) -> I ()
tryParse p s ok bad =
  case parse p "" s of
    Right _ -> ok
    Left  e -> bad e

tryCompile :: String -> I (Either SomeException ([LDef], TCState))
tryCompile file = trySIO $ compile file

compile :: String -> I ([LDef], TCState)
compile file = do
--  putStrLnI $ "tryCompile:\n" ++ file
  let mdl@(EModule mn es _) = parseDie pTopModule "" file
  defs <- updateTCStateCache mdl
  (_, tcstate, _) <- gets isCComp
  let mdl' = EModule mn es (SetTCState (tcStateToXTCState tcstate) : defs)
  flgs <- gets isFlags
  cash <- gets isCache
--  putStrLnI $ " tryCompile compile " ++ show mdl'
  ((dmdl, _, tcstate'), _) <- liftIO $ runStateIO (compileInteractive flgs mdl') cash
  cmdl <- liftIO $ evaluate $ compileToCombinators dmdl
--  putStrLnI $ " tryCompile dmdl = " ++ (show $ tBindingsOf dmdl)
  return (tBindingsOf cmdl, tcstate')

evalExpr :: [LDef] -> I ()
evalExpr cmdl = do
--  putStrLnI $ "evalExpr: " ++ show cmdl
  (_, _, tmap) <- gets isCComp
  let ares = translateWithMap tmap (cmdl, Var $ mkIdent (interactiveName ++ "." ++ itIOName))
      res = unsafeCoerce ares :: IO ()
  mval <- liftIO $ try (seq res (return res))
  case mval of
    Left  e -> err e
    Right val -> do
      mio <- liftIO $ try val
      case mio of
        Left  e -> err e
        Right _ -> return ()

showType :: String -> I ()
showType line = do
  ls <- gets isLines
  res <- tryCompile (ls ++ "\n" ++ mkIt line)
  case res of
    Right (_, tcs) -> do
      case stLookup "" (mkIdent itName) (valueTable tcs) of
        Right (Entry _ t) -> putStrLnI $ showEType t
        _ -> error "showType"
    Left  e -> err e

showKind :: String -> I ()
showKind line = do
  ls <- gets isLines
  res <- tryCompile (ls ++ "\n" ++ mkTypeIt line)
  case res of
    Right (_, tcs) -> do
      case stLookup "" (mkIdent itTypeName) (typeTable tcs) of
        Right (Entry _ t) -> putStrLnI $ showEType t
        _ -> error "showKind"
    Left  e -> err e

runMain :: String -> I ()
runMain line = oneline $ "System.IO.PrintOrRun._withArgs " ++ show (words line) ++ " main"

showDefs :: I ()
showDefs = do
  ls <- gets isLines
  putStrLnI ls

saveDefs :: String -> I ()
saveDefs "" = saveDefs (interactiveName ++ ".hs")
saveDefs line = do
  ls <- gets isLines
  liftIO $ writeFile line ls
  putStrLnI $ "wrote " ++ line

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

updateTCStateCache :: EModule -> I [EDef]
updateTCStateCache (EModule mn es ds) = do
  let isImport (Import _) = True
      isImport _ = False
      (imps, notImps) = partition isImport ds
      nImps = length imps
      mdl = EModule mn es imps
  (oNImps, _, _) <- gets isCComp
  -- Imports can only be added and deleted, so if it's the same
  -- number of imports as before, then they must be identical.
  if nImps == oNImps then
    return ()
   else do
    -- imports have changed, update TCState cache
    flgs <- gets isFlags
    cash <- gets isCache
--    putStrLnI "*** update tcstate"
    let mdl' = addPreludeImport mdl
    ((_, syms, tcstate), ch) <- liftIO $ runStateIO (compileInteractive flgs mdl') cash
    let idmap = translateMap $ concatMap tBindingsOf $ cachedModules ch
--    putStrLnI $ "*** update isFast " ++ show nImps
    modify $ \ is -> is{ isCComp = (nImps, tcstate, idmap), isCache = ch, isSymbols = syms }
  return notImps

getEditor :: I String
getEditor = fromMaybe defaultEditor <$> gets (editor . isFlags)

edit :: String -> I ()
edit s = do
  ed <- getEditor
  case s of
    "" -> do
      line <- gets isErrLine
      file <- gets isErrFile
      _ <- liftIO $ system $ printf ed line file
      return ()
    file -> do
      _ <- liftIO $ system $ printf ed (1::Int) file
      return ()
  reload

alt :: Either a b -> Either a b -> Either a b
alt (Left _) r = r
alt r _ = r

finds :: String -> I ()
finds str = do
  (ts, vs) <- gets isSymbols
  let i = mkIdent str
  case stLookup "type" i vs `alt` stLookup "value" i ts of
    Left s -> putStrLnI s
    Right (Entry (EVar qi) _) | let loc@(SLoc f l _) = slocIdent qi, not (isNoSLoc loc) -> do
      ed <- getEditor
      _ <- liftIO $ system $ printf ed l f
      reload
      return ()
    _ -> putStrLnI "Unknown location"
