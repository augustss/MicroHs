module MicroHs.Interactive(module MicroHs.Interactive) where
import Prelude
import Data.List
import Control.Exception
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.Desugar(LDef)
import MicroHs.Expr(EType, showEType)
import MicroHs.Flags
import MicroHs.Ident(mkIdent, Ident)
import qualified MicroHs.IdentMap as M
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.SymTab(Entry(..))
import MicroHs.Translate
import MicroHs.TypeCheck(ValueExport(..), TypeExport(..), TModule(..))
import Unsafe.Coerce
import System.Console.SimpleReadline
import Compat
import MicroHs.Instances(compiledWithGHC)

type IState = (String, Flags, Cache)

type I a = StateIO IState a

mainInteractive :: Flags -> IO ()
mainInteractive flags = do
--  when (not usingMhs) $
--    error "Interactive mhs not available when compiled with ghc"
  putStrLn "Welcome to interactive MicroHs!"
  let flags' = flags{ loading = True }
  cash <- getCached flags'
  _ <- runStateIO start (preamble, flags', cash)
  return ()

preamble :: String
preamble = "module " ++ interactiveName ++ "(module " ++ interactiveName ++
           ") where\nimport Prelude\nimport System.IO.PrintOrRun\ndefault (Integer, Double, String)\n"

start :: I ()
start = do
  reload
  liftIO $ putStrLn "Type ':quit' to quit, ':help' for help"
  when compiledWithGHC $
    liftIO $ putStrLn "WARNING: Compiled with GHC, so limited functionality."
  repl

repl :: I ()
repl = do
  ms <- liftIO $ getInputLineHist ".mhsi" "> "
  case ms of
    Nothing -> repl
    Just s ->
      case s of
        [] -> repl
        ':':r -> do
          c <- command r
          if c then repl else liftIO $ putStrLn "Bye"
        _ -> do
          oneline s
          repl

command :: String -> I Bool
command s =
  case words s of
    [] -> return True
    c : ws ->
      case filter (isPrefixOf c . fst) commands of
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
  [ ("quit", const $ return False)
  , ("clear", const $ do
      updateLines (const preamble)
      modify $ \ (ls, flgs, _) -> (ls, flgs, emptyCache)
      return True
    )
  , ("reload", const $ do
      (ls, flgs, cash) <- get
      cash' <- liftIO $ validateCache flgs cash
      put (ls, flgs, cash')
      reload
      return True
    )
  , ("delete", \ del -> do
      updateLines (unlines . filter (not . isPrefixOf del) . lines)
      return True
    )
  , ("type", \ line -> do
      showType line
      return True
    )
  , ("kind", \ line -> do
      showKind line
      return True
    )
  , ("help", \ _ -> do
      liftIO $ putStrLn helpText
      return True
    )
  ]

reload :: I ()
reload = do
  (ls, _, _) <- get
  rld <- tryCompile ls   -- reload modules right away
  case rld of
    Left msg -> liftIO $ err msg
    Right _  -> return ()

helpText :: String
helpText = "\
  \Commands:\n\
  \:quit      quit MicroHs\n\
  \:reload    reload modules\n\
  \:clear     clear all definitions\n\
  \:delete d  delete definition(s) d\n\
  \:type e    show type of e\n\
  \:kind t    show type of t\n\
  \:help      this text\n\
  \expr       evaluate expression\n\
  \defn       add top level definition\n\
  \"

updateLines :: (String -> String) -> I ()
updateLines f = modify $ \ (ls, flgs, cash) -> (f ls, flgs, cash)

updateCache :: (Cache -> Cache) -> I ()
updateCache f = modify $ \ (ls, flgs, cash) -> (ls, flgs, f cash)

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

mkItIO :: String -> String
mkItIO l =
  mkIt l ++
  itIOName ++ " = printOrRun " ++ itName ++ "\n"

mkTypeIt :: String -> String
mkTypeIt l =
  "type " ++ itTypeName ++ " = " ++ l ++ "\n"

err :: Exn -> IO ()
err e = err' $ exnToString e

err' :: String -> IO ()
err' s = putStrLn $ "Error: " ++ s

oneline :: String -> I ()
oneline line = do
  (ls, _, _) <- get
  let lls = ls ++ line ++ "\n"
      def = do
        defTest <- tryCompile lls
        case defTest of
          Right _ -> updateLines (const lls)
          Left  e -> liftIO $ err e
      expr = do
        exprTest <- tryCompile (ls ++ "\n" ++ mkItIO line)
        case exprTest of
          Right m -> evalExpr m
          Left  e -> liftIO $ err e
  -- First try to parse as a definition,
  tryParse pTop lls def $ \ _ ->
    -- if that fails, parse as an expression.
    tryParse pExprTop line expr $
      liftIO . err'

tryParse :: forall a . Show a => P a -> String -> I () -> (String -> I ()) -> I ()
tryParse p s ok bad =
  case parse p "" s of
    Right _ -> ok
    Left  e -> bad e

tryCompile :: String -> I (Either Exn [LDef])
tryCompile file = do
  updateCache (deleteFromCache interactiveId)
  (_, flgs, cash) <- get
  liftIO $ writeFile (interactiveName ++ ".hs") file
  res <- liftIO $ try $ compileCacheTop flgs interactiveId cash
  case res of
    Left e -> return (Left e)
    Right (m, cash') -> do
      updateCache (const cash')
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
  (ls, _, _) <- get
  res <- tryCompile (ls ++ "\n" ++ mkIt line)
  case res of
    Right _ -> do
      (_, _, cash) <- get
      let t = getTypeInCache cash (mkIdent itName)
      liftIO $ putStrLn $ showEType t
    Left  e ->
      liftIO $ err e

showKind :: String -> I ()
showKind line = do
  (ls, _, _) <- get
  res <- tryCompile (ls ++ "\n" ++ mkTypeIt line)
  case res of
    Right _ -> do
      (_, _, cash) <- get
      let t = getKindInCache cash (mkIdent itTypeName)
      liftIO $ putStrLn $ showEType t
    Left  e ->
      liftIO $ err e

getCModule :: Cache -> CModule
getCModule cash =
  case M.lookup interactiveId (cache cash) of
    Nothing -> undefined   -- this cannot happen
    Just cm -> cm

getTypeInCache :: Cache -> Ident -> EType
getTypeInCache cash i =
  case tModuleOf (getCModule cash) of
    TModule _ _ _ _ _ _ vals _ ->
      head $ [ t | ValueExport i' (Entry _ t) <- vals, i == i' ] ++ [undefined]

getKindInCache :: Cache -> Ident -> EType
getKindInCache cash i =
  case tModuleOf (getCModule cash) of
    TModule _ _ tys _ _ _ _ _ ->
      head $ [ k | TypeExport i' (Entry _ k) _ <- tys, i == i' ] ++ [undefined]
