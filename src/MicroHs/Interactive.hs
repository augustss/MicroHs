module MicroHs.Interactive(mainInteractive, mainEvalArg) where
import qualified Prelude(); import MHSPrelude
import Data.Char
import Data.List
import Data.Maybe
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.Desugar(LDef)
import MicroHs.Error(mhsError, errLoc, formatMhsError)
import MicroHs.Exp(Exp(Var))
import MicroHs.Expr(showEType, EModule(..), EDef(..), Expr(EVar))
import MicroHs.Flags
import MicroHs.Ident(mkIdent, Ident, unIdent, isIdentChar, {-SLoc(..),-} slocIdent, isNoSLoc, unQualString)
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
import System.IO.TimeMilli
import Text.Printf
--import System.IO.TimeMilli

defaultEditor :: String
defaultEditor = "vim +%d %s"

data IState = IState {
  isLines   :: String,
  isFlags   :: Flags,
  isCache   :: Cache,
  isSymbols :: Symbols,
  isStats   :: Bool,
  isCompStats :: Bool,
  isType    :: Bool,
  isColor   :: Bool,
  isPrompt  :: String,
  isPrompt2 :: String,
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
  when (verbosityGT flags (-1)) $ do
    putStrLn $ "Welcome to interactive MicroHs, version " ++ showVersion version
    putStrLn $ "Integer implemented with " ++
      if wantGMP then "GMP" else if wantImath then "imath" else "Haskell"
{-
  bootTime <- getBootTimeMicro
  now <- getTimeMicro
  putStrLn $ "time=" ++ show (now - bootTime) ++ "us"
-}
  mhome <- lookupEnv "HOME"
  let flags' = flags{ loading = verbosityGT flags (-1) }
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
  ccash <- getCached flags
  cash <- loadEmbedded flags ccash
  let startMdl = preamble ++ unlines (map ("import " ++) mdls)
{-
  bootTime <- getBootTimeMicro
  now <- getTimeMicro
  putStrLn $ "time=" ++ show (now - bootTime) ++ "us"
-}
  return $ IState { isLines = startMdl, isFlags = flags, isCache = cash
                  , isSymbols = noSymbols, isStats = False, isCompStats = False
                  , isType = False, isColor = False
                  , isPrompt = "mhs> ", isPrompt2 = "mhs| ", isCComp = (-1, error "tcstate", translateMapEmpty)
                  , isHistory = hist, isErrLine = 1, isErrFile = "" }

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
  let line "" = return ()
      line (':':r) = do _ <- command r; return ()
      line s = oneline s
  rc <- liftIO (catch (lines <$> readFile ".mhsi_rc") (\ (_ :: SomeException) -> return []))
  mapM_ line rc
  when (verbosityGT (isFlags is) (-1)) $
    putStrLnI "Type ':quit' to quit, ':help' for help"
{-
  liftIO $ do bootTime <- getBootTimeMicro
              now <- getTimeMicro
              putStrLn $ "time=" ++ show (now - bootTime) ++ "us"
-}
  repl

getLineI :: String -> I (Maybe String)
getLineI prompt = do
  mdls <- gets (cachedModuleNames . isCache)
  syms <- gets isSymbols
  stdinFlag <- gets (useStdin . isFlags)
  hist <- gets isHistory
  liftIO $
    if stdinFlag then do
      putStr prompt
      hFlush stdout
      es <- try getLine
      case es of
        Left  (_::SomeException) -> return Nothing
        Right s                  -> return (Just s)
    else do
      ms <- getInputLineHistComp (return . complete mdls syms) hist prompt
      return (ms <|> Just "")  -- ignore ^D

evalLine :: String -> I ()
evalLine s = do
  t1 <- liftIO getTimeMilli
  oneline s
  t2 <- liftIO getTimeMilli
  cstat <- gets isCompStats
  when cstat $
    putStrLnI $ "(" ++ show (t2 - t1) ++ "ms compile)"

repl :: I ()
repl = do
  prompt <- gets isPrompt
  ms <- getLineI prompt
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
          evalLine s
          repl

multiline :: I ()
multiline = loop []
  where
    loop acc = do
      prompt <- gets isPrompt2
      ms <- getLineI prompt
      case ms of
        Nothing -> return ()
        Just s
          | trim s == ":}" ->
            let blk = unlines (reverse acc)
            in  unless (all isSpace blk) $ evalLine blk
          | otherwise -> loop (s : acc)
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

command :: String -> I Bool
command s = do
  let fw = (!!0) . words . fst
      -- split string into command and rest
      (c, rest) = break isSpace $ dropWhile isSpace s
      arg = dropWhile isSpace rest
  if null c then
    return True
   else
    case filter (isPrefixOf c . fw) commands of
      [] -> do
        putStrLnI "Unrecognized command"
        return True
      [(_, cmd)] -> do
        cmd arg
      xs -> do
        putStrLnI $ "Ambiguous command: " ++ unwords (map fw xs)
        return True

commands :: [(String, String -> I Bool)]
commands =
  [ ("quit        quit MicroHs",
     const $ return False
    )
  , ("{           start multi-line input (end with :})", const $ do
      multiline
      return True
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
  , ("set [OPT]   (un)set option", \ line -> do
      setOptions line
      return True
    )
  ]

help :: String -> I Bool
help = const $ do
  putStrLnI $ helpText ++ unlines (map ((':' :) . fst) commands)
  return True

setOptions :: String -> I ()
setOptions "" = do
  stats <- gets isStats
  compStats <- gets isCompStats
  prompt <- gets isPrompt
  prompt2 <- gets isPrompt2
  typ <- gets isType
  color <- gets isColor
  flags <- gets isFlags
  putStrLnI "Current flags: (use + to set and - to unset)"
  putStrLnI $ "  " ++ (if stats     then "+" else "-") ++ "s"
  putStrLnI $ "  " ++ (if compStats then "+" else "-") ++ "c"
  putStrLnI $ "  " ++ (if typ       then "+" else "-") ++ "t"
  putStrLnI $ "  " ++ (if color     then "+" else "-") ++ "color"
  putStrLnI $ "  path=" ++ intercalate ":" (srcPaths flags)
  putStrLnI $ "  prompt=" ++ prompt
  putStrLnI $ "  prompt-cont=" ++ prompt2
setOptions "+s" = do
  modify $ \ is -> is{ isStats = True }
setOptions "-s" = do
  modify $ \ is -> is{ isStats = False }
setOptions "+c" = do
  modify $ \ is -> is{ isCompStats = True }
setOptions "-c" = do
  modify $ \ is -> is{ isCompStats = False }
setOptions "+t" = do
  modify $ \ is -> is{ isType = True }
setOptions "-t" = do
  modify $ \ is -> is{ isType = False }
setOptions "+color" = do
  modify $ \ is -> is{ isColor = True }
setOptions "-color" = do
  modify $ \ is -> is{ isColor = False }
setOptions ('p':'a':'t':'h':'=':p) =
  modify $ \ is -> is{ isFlags = (isFlags is){ srcPaths = splitColonPath p } }
setOptions ('p':'r':'o':'m':'p':'t':'=':p) =
  modify $ \ is -> is{ isPrompt = p }
setOptions ('p':'r':'o':'m':'p':'t':'-':'c':'o':'n':'t':'=':p) =
  modify $ \ is -> is{ isPrompt2 = p }
setOptions _ =
  putStrLnI "Unknown flag.  Known flags: +s, -s, +c, -c, +color, -color, path=PATH, prompt=STR, prompt2=STR"

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

interactiveName :: String
interactiveName = "Interactive"

itName :: String
itName = "_it"

itTypeName :: String
itTypeName = "Type_it"

itIOName :: String
itIOName = "_itIO"

mkIt :: String -> String
mkIt l =
  itName ++ " =\n" ++ unlines (map ("  " ++) (lines l))

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
err e =
  case fromException e of
    Just e' -> do
      case errLoc e' of
        Just loc@(SLoc f l _) | not (isNoSLoc loc) -> modify $ \ is -> is{ isErrFile = f, isErrLine = l }
        _ -> return ()
      color <- gets isColor
      err' $ formatMhsError color e'
    _  -> err' =<< wrapColor 91 ("*** Exception: " ++ displayException e)

err' :: String -> I ()
err' s = liftIO $ putStrLn s

wrapColor :: Int -> String -> I String
wrapColor c s = do
  color <- gets isColor
  return $ if color then "\ESC[" ++ show c ++ "m" ++ s ++ "\ESC[0m" else s

unindent :: String -> String
unindent s =
  let ls = lines s
      nonEmpty = filter (not . all isSpace) ls
  in  case nonEmpty of
        [] -> ""
        _  ->
          let n = minimum (map (length . takeWhile (== ' ')) nonEmpty)
          in  unlines (map (drop n) ls)

stripLet :: String -> Maybe String
stripLet ('l':'e':'t':r) | null r || isSpace (head r) = Just (unindent ("   " ++ r))
stripLet _ = Nothing

oneline :: String -> I ()
oneline aline = do
  let line = unindent aline
      exprInput =
        case lines line of
          [l] -> l
          ls' -> "{\n" ++ unlines (map ("  " ++) ls') ++ "}"
  stats <- gets isStats
  let def lls = do
        defTest <- tryCompile lls
        case defTest of
          Right _ -> do
            updateLines (const lls)
          Left  e -> err e
      expr ln = do
--        t1 <- liftIO getTimeMilli
        flgs <- gets isFlags
        ls <- gets isLines
        exprTest <- tryCompile (ls ++ "\n" ++ mkItIO flgs stats ln)
        case exprTest of
          Right (m, _) -> do
            evalExpr ln m
{-
            t2 <- liftIO getTimeMilli
            when (stats) $
              liftIO $ putStrLn $ "total " ++ show (t2 - t1) ++ "ms"
-}
          Left  e -> do
            case fromException e of
              Just (MhsUndefError _ _ i) | let u = unQualString i, i /= u -> do
                -- We have an undefined qualified identifier.
                -- Add a qualified import for the module (if it doesn't exist),
                -- if that succeeds, rerun the expression.
                let mdl = fromJust $ stripSuffix ("." ++ u) i
                    imp = "import qualified " ++ mdl
                if imp `elem` lines ls then
                  -- Already imported
                  err e
                 else do
                  let lls = ls ++ imp ++ "\n"
                  defTest <- tryCompile lls
                  case defTest of
                    Right _ -> do
                      -- Adding the import succeeded, so try again
                      updateLines (const lls)
                      expr ln
                    Left  _ -> err e
              _ -> err e
  -- A small hack, if we import something we have already imported
  -- in exactly the same way, we just reload instead.  This is to avoid
  -- building up multiple imports.
  ls <- gets isLines
  let trimmed = dropWhile isSpace aline
  if take 1 (words line) == ["import"] && trimmed `elem` lines ls then
    reload
   else
    -- First try to parse as a definition,
    tryParse pTopModule (ls ++ line) def $ \ _ ->
      -- if that fails, parse as an expression.
      tryParse pExprTop exprInput (const $ expr line) $ \ msg ->
        -- finally, try to strip an initial 'let' and parse as a definition
        let emsg = err' msg in
        case stripLet line of
          Just line' -> tryParse pTopModule (ls ++ line') def $ const emsg
          Nothing    -> emsg

tryParse :: forall a . -- Show a =>
            P a -> String -> (String -> I ()) -> (String -> I ()) -> I ()
tryParse p s ok bad =
  case parse p "" s of
    Right _ -> ok s
    Left  e -> bad (show e)

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

evalExpr :: String -> [LDef] -> I ()
evalExpr ln cmdl = do
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
        Right _ -> do
          typ <- gets isType
          when typ $ do
            liftIO $ putStr " :: "
            showType ln

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
    Left s -> do
      color <- gets isColor
      putStrLnI $ formatMhsError color s
    Right (Entry (EVar qi) _) | let loc@(SLoc f l _) = slocIdent qi, not (isNoSLoc loc) -> do
      ed <- getEditor
      _ <- liftIO $ system $ printf ed l f
      reload
      return ()
    _ -> putStrLnI "Unknown location"
