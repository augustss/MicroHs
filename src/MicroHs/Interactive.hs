module MicroHs.Interactive(module MicroHs.Interactive) where
import Prelude
--Ximport Data.List
import Control.DeepSeq
import Control.Exception
import qualified MicroHs.StateIO as S
import MicroHs.Compile
import MicroHs.Exp(Exp)
import MicroHs.Ident(Ident, mkIdent)
import MicroHs.Parse
import MicroHs.Translate
import Unsafe.Coerce
import System.Console.SimpleReadline
--Ximport Compat

type LDef = (Ident, Exp)  -- XXX why?

type IState = (String, Flags, Cache)

type I a = S.StateIO IState a

mainInteractive :: Flags -> IO ()
mainInteractive (Flags a b c d _) = do
  putStrLn "Welcome to interactive MicroHs!"
  putStrLn "Type ':quit' to quit, ':help' for help"
  let flags' = Flags a b c d True
  _ <- S.runStateIO repl (preamble, flags', emptyCache)
  return ()

preamble :: String
preamble = "module " ++ interactiveName ++ "(module " ++ interactiveName ++
           ") where\nimport Prelude\nimport Unsafe.Coerce\n"

repl :: I ()
repl = S.do
  ms <- S.liftIO $ getInputLineHist ".mhsi" "> "
  case ms of
    Nothing -> repl
    Just s ->
      case s of
        [] -> repl
        ':':r -> S.do
          c <- command r
          if c then repl else S.liftIO $ putStrLn "Bye"
        _ -> S.do
          oneline s
          repl

command :: String -> I Bool
command s =
  case words s of
    [] -> S.return True
    c : ws ->
      case filter (isPrefixOf c . fst) commands of
        [] -> S.do
          S.liftIO $ putStrLn "Unrecognized command"
          S.return True
        [(_, cmd)] ->
          cmd (unwords ws)
        xs -> S.do
          S.liftIO $ putStrLn $ "Ambiguous command: " ++ unwords (map fst xs)
          S.return True

commands :: [(String, String -> I Bool)]
commands =
  [ ("quit", const $ S.return False)
  , ("clear", const $ S.do
      updateLines (const preamble)
      S.modify $ \ (ls, flgs, _) -> (ls, flgs, emptyCache)
      S.return True
    )
  , ("reload", const $ S.do
      S.modify $ \ (ls, flgs, _) -> (ls, flgs, emptyCache)
      S.return True
    )
  , ("delete", \ del -> S.do
      updateLines (unlines . filter (not . isPrefixOf del) . lines)
      S.return True
    )
  , ("help", \ _ -> S.do
      S.liftIO $ putStrLn helpText
      S.return True
    )
  ]

helpText :: String
helpText = "Commands:\n  :quit      quit MicroHs\n  :clear     clear all definitions\n  :delete d  delete definition(s) d\n  :help      this text\n  expr       evaluate expression\n  defn       add top level definition\n"

updateLines :: (String -> String) -> I ()
updateLines f = S.modify $ \ (ls, flgs, cache) -> (f ls, flgs, cache)

interactiveName :: String
interactiveName = "Interactive"

itName :: String
itName = "_it"

mkIt :: String -> String
mkIt l = itName ++ " :: Any\n" ++ itName ++ " = unsafeCoerce (" ++ l ++ ")\n"

err :: Exn -> IO ()
err (Exn s) = putStrLn $ "Error: " ++ s

oneline :: String -> I ()
oneline line = S.do
  (ls, _, _) <- S.get
  case parse pExprTop "" line of
    Right _ -> S.do
      -- Looks like an expressions, make it a definition
      exprTest <- tryCompile (ls ++ "\n" ++ mkIt line)
      case exprTest of
        Right m -> evalExpr m
        Left  e -> S.liftIO $ err e
    Left _ -> S.do
      -- Not an expression, try adding it as a definition
      let lls = ls ++ line ++ "\n"
      defTest <- tryCompile lls
      case defTest of
        Right _ -> updateLines (const lls)
        Left  e -> S.liftIO $ err e

tryCompile :: String -> I (Either Exn [LDef])
tryCompile file = S.do
  (ls, flgs, cache) <- S.get
  let
    iid = mkIdent interactiveName
  S.liftIO $ writeFile (interactiveName ++ ".hs") file
  res <- S.liftIO $ try $ compileCacheTop flgs iid cache
  case res of
    Left e -> S.return (Left e)
    Right (m, cache') -> S.do
      S.put (ls, flgs, deleteFromCache iid cache')
      S.return (Right m)

evalExpr :: [LDef] -> I ()
evalExpr cmdl = S.do
  let res = translate (mkIdent (interactiveName ++ "." ++ itName), cmdl)
  mval <- S.liftIO $ try (seq res (return res))
  S.liftIO $
    case mval of
      Left  e -> err e
      Right val ->
        if primIsInt val then
          putStrLn $ showInt $ unsafeCoerce val
        else do
          putStrLn "Warning: not an Int"
          mio <- try (print (force ((unsafeCoerce val)::Int)))
          case mio of
            Left  e -> err e
            Right _ -> return ()
