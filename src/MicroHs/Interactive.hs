module MicroHs.Interactive(module MicroHs.Interactive) where
import Prelude
import Data.List
import Control.Exception
import MicroHs.Compile
import MicroHs.Desugar(LDef)
import MicroHs.Flags
import MicroHs.Ident(mkIdent, Ident)
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.Translate
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
           ") where\nimport Prelude\nimport System.IO.PrintOrRun\ndefault (Integer, Double)\n"

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

updateCache :: (Cache -> Cache) -> I ()
updateCache f = modify $ \ (ls, flgs, cache) -> (ls, flgs, f cache)

interactiveName :: String
interactiveName = "Interactive"

interactiveId :: Ident
interactiveId = mkIdent interactiveName

itName :: String
itName = "_it"

itIOName :: String
itIOName = "_itIO"

mkIt :: String -> String
mkIt l =
  itName   ++ " = (" ++ l ++ ")\n" ++
  itIOName ++ " = printOrRun " ++ itName ++ "\n"

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
        exprTest <- tryCompile (ls ++ "\n" ++ mkIt line)
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
  r <- tryCompile' file
  updateCache (deleteFromCache interactiveId)
  return r

tryCompile' :: String -> I (Either Exn [LDef])
tryCompile' file = do
  (_, flgs, cache) <- get
  liftIO $ writeFile (interactiveName ++ ".hs") file
  res <- liftIO $ try $ compileCacheTop flgs interactiveId cache
  case res of
    Left e -> return (Left e)
    Right (m, cache') -> do
      updateCache (const cache')
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
