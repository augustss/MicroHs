module MicroHs.Interactive(module MicroHs.Interactive) where
import Prelude
--Ximport Data.List
--import Control.DeepSeq
import Control.Exception
import MicroHs.StateIO
import MicroHs.Compile
import MicroHs.Desugar(LDef)
import MicroHs.Ident(mkIdent)
import MicroHs.Parse
import MicroHs.Translate
import Unsafe.Coerce
import System.Console.SimpleReadline
--Ximport Compat

type IState = (String, Flags, Cache)

type I a = StateIO IState a

mainInteractive :: Flags -> IO ()
mainInteractive (Flags a b c d _) = do
  putStrLn "Welcome to interactive MicroHs!"
  putStrLn "Type ':quit' to quit, ':help' for help"
  let flags' = Flags a b c d True
  _ <- runStateIO start (preamble, flags', emptyCache)
  return ()

preamble :: String
preamble = "module " ++ interactiveName ++ "(module " ++ interactiveName ++
           ") where\nimport Prelude\ndefault (Integer, Double)\n"

start :: I ()
start = do
  reload
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
      modify $ \ (ls, flgs, _) -> (ls, flgs, emptyCache)
      reload
      return True
    )
  , ("delete", \ del -> do
      updateLines (unlines . filter (not . isPrefixOf del) . lines)
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
helpText = "Commands:\n  :quit      quit MicroHs\n  :reload    reload modules\n  :clear     clear all definitions\n  :delete d  delete definition(s) d\n  :help      this text\n  expr       evaluate expression\n  defn       add top level definition\n"

updateLines :: (String -> String) -> I ()
updateLines f = modify $ \ (ls, flgs, cache) -> (f ls, flgs, cache)

interactiveName :: String
interactiveName = "Interactive"

itName :: String
itName = "_it"

mkIt :: String -> String
mkIt l = itName ++ " :: IO ()\n" ++ itName ++ " = printOrRun (" ++ l ++ ")\n"

err :: Exn -> IO ()
err (Exn s) = putStrLn $ "Error: " ++ s

oneline :: String -> I ()
oneline line = do
  (ls, _, _) <- get
  case parse pExprTop "" line of
    Right _ -> do
      -- Looks like an expressions, make it a definition
      exprTest <- tryCompile (ls ++ "\n" ++ mkIt line)
      case exprTest of
        Right m -> evalExpr m
        Left  e -> liftIO $ err e
    Left _ -> do
      -- Not an expression, try adding it as a definition
      let lls = ls ++ line ++ "\n"
      defTest <- tryCompile lls
      case defTest of
        Right _ -> updateLines (const lls)
        Left  e -> liftIO $ err e

tryCompile :: String -> I (Either Exn [LDef])
tryCompile file = do
  (ls, flgs, cache) <- get
  let
    iid = mkIdent interactiveName
  liftIO $ writeFile (interactiveName ++ ".hs") file
  res <- liftIO $ try $ compileCacheTop flgs iid cache
  case res of
    Left e -> return (Left e)
    Right (m, cache') -> do
      put (ls, flgs, deleteFromCache iid cache')
      return (Right m)

evalExpr :: [LDef] -> I ()
evalExpr cmdl = do
  let ares = translate (mkIdent (interactiveName ++ "." ++ itName), cmdl)
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
