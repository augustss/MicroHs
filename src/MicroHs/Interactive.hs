module MicroHs.Interactive(module MicroHs.Interactive) where
import Prelude
import Control.Exception
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
  putStrLn "Welcome to interactive MicroHs!"
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
mkIt l = itName ++ " :: Any\n" ++ itName ++ " = unsafeCoerce (" ++ l ++ ")\n"

oneline :: String -> I ()
oneline line = S.do
  (ls, flgs) <- S.get
  exprTest <- tryCompile (ls ++ "\n" ++ mkIt line)
  case exprTest of
    Right m -> evalExpr m
    Left  _ -> S.do
      let lls = ls ++ line ++ "\n"
      defTest <- tryCompile lls
      case defTest of
        Right      _ -> S.put (lls, flgs)
        Left (Exn s) -> S.liftIO $ putStrLn s

tryCompile :: String -> I (Either Exn [LDef])
tryCompile file = S.do
  S.liftIO $ writeFile (interactiveName ++ ".hs") file
  flgs <- S.gets snd
  S.liftIO $ try $ compileTop flgs (mkIdent interactiveName)

evalExpr :: [LDef] -> I ()
evalExpr cmdl = S.do
  let res = translate (mkIdent (interactiveName ++ "." ++ itName), cmdl)
      err s = putStrLn $ "Error: " ++ s
  mval <- S.liftIO $ try (seq res (return res))
  S.liftIO $
    case mval of
      Left (Exn s) -> err s
      Right val ->
        if primIsInt val then
          putStrLn $ showInt $ unsafeCoerce val
        else if primIsIO val then do
          mio <- try (unsafeCoerce val)
          case mio of
            Left (Exn s) -> err s
            Right _ -> return ()
        else
          putStrLn "Type must be Int or IO"
