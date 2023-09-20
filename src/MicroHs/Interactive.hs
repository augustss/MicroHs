module MicroHs.Interactive(module MicroHs.Interactive) where
import Prelude
import qualified MicroHs.StateIO as S
import MicroHs.Compile
import MicroHs.Exp(Exp)
import MicroHs.Ident(Ident, mkIdent)
import MicroHs.Translate
import Unsafe.Coerce
import System.Console.SimpleReadline
--Ximport Compat

type LDef = (Ident, Exp)  -- XXX why?

type IState = (String, Flags)

type I a = S.StateIO IState a

mainInteractive :: Flags -> IO ()
mainInteractive flags = do
  putStrLn "Type ':quit' to quit"
  _ <- S.runStateIO repl (preamble, flags)
  return ()

preamble :: String
preamble = "module " ++ interactiveName ++ "(module " ++ interactiveName ++ ") where\nimport Prelude\nimport Unsafe.Coerce\n"

repl :: I ()
repl = S.do
  ms <- S.liftIO $ getInputLineHist ".mhsi" "> "
  case ms of
    Nothing -> repl
    Just ":quit" -> S.liftIO $ putStrLn "Bye"
    Just s -> S.do
      oneline s
      repl

interactiveName :: String
interactiveName = "Interactive"

itName :: String
itName = "_it"

mkIt :: String -> String
mkIt l = itName ++ " :: Any\n" ++ itName ++ " = unsafeCoerce (" ++ l ++ ")"

oneline :: String -> I ()
oneline line = S.do
  (ls, flgs) <- S.get
  exprTest <- tryCompile (ls ++ "\n" ++ mkIt line)
  case exprTest of
    Right m -> evalExpr m
    Left  _ -> S.do
      let lls = ls ++ "\n" ++ line
      defTest <- tryCompile lls
      case defTest of
        Right _ -> S.put (lls, flgs)
        Left  s -> S.liftIO $ putStrLn s

tryCompile :: String -> I (Either String [LDef])
tryCompile file = S.do
  S.liftIO $ writeFile (interactiveName ++ ".hs") file
  flgs <- S.gets snd
  cmdl <- S.liftIO $ compileTop flgs (mkIdent interactiveName)
  S.return (Right cmdl)

evalExpr :: [LDef] -> I ()
evalExpr cmdl = S.do
  let val = translate (mkIdent (interactiveName ++ "." ++ itName), cmdl)
  S.liftIO $
    if primIsInt val then
      putStrLn $ showInt $ unsafeCoerce val
{-
    else if primIsIO val then
      unsafeCoerce val
-}
    else
      putStrLn "Type must be Int or IO"
