module MicroHs.Interactive(module MicroHs.Interactive) where
import Prelude
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
mainInteractive flags = do
  putStrLn "Welcome to interactive MicroHs!"
  putStrLn "Type ':quit' to quit"
  _ <- S.runStateIO repl (preamble, flags, emptyCache)
  return ()

preamble :: String
preamble = "module " ++ interactiveName ++ "(module " ++ interactiveName ++ ") where\nimport Prelude\nimport Unsafe.Coerce\n"

repl :: I ()
repl = S.do
  ms <- S.liftIO $ getInputLineHist ".mhsi" "> "
  case ms of
    Nothing -> repl
    Just ":quit" -> S.liftIO $ putStrLn "Bye"
    Just ":clear" -> S.do
      updateLines (const preamble)
      repl
    Just s | Just del <- stripPrefixBy eqChar ":del " s -> S.do
      updateLines (unlines . filter (not . isPrefixOfBy eqChar del) . lines)
      repl
    Just s -> S.do
      oneline s
      repl

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
          mio <- try (print ((unsafeCoerce val)::Int))
          case mio of
            Left  e -> err e
            Right _ -> return ()
