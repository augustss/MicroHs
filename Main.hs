import Data.Maybe
import System.Environment(getArgs)
import Parse
import Desugar
import Exp

primOps :: [(Ident, Exp)]
primOps =
  [ ("error", Prim "?")
  , ("+",     Prim "+")
  , ("-",     Prim "-")
  , ("-'",    Prim "-'")
  , ("*",     Prim "*")
  , ("quot",  Prim "/")
  , ("rem",   Prim "%")
  , ("<",     Prim "<")
  , ("<=",    Prim "<=")
  , (">",     Prim ">")
  , (">=",    Prim ">=")
  , ("==",    Prim "==")
  , ("/=",    Prim "!=")
  ]

main :: IO ()
main = do
  [fn] <- getArgs
  mdl <- parseDie pTop fn <$> readFile fn
  let ds = desugar mdl
      ds' = [ (n, compileOpt e) | (n, e) <- ds ]
      defs :: [(Ident, Exp)]
      defs = [ (n, ref i) | ((n, _), i) <- zip ds' [0..] ] ++ primOps
      find n = fromMaybe (error $ "undefined: " ++ show n) $ lookup n defs
      emain = find "main"
      res = foldr def emain (zip ds' [0..])
      def :: ((Ident, Exp), Int) -> Exp -> Exp
      def ((n, e), i) r = App2 CT (Lbl i (subst e)) r
      subst (Var n) = find n
      subst (App f a) = App (subst f) (subst a)
      subst e = e
      ref i = Var $ "_" ++ show i
--  mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toString e) ds
  mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toString e) ds'
  putStrLn $ toStringP res
  writeFile "out.comb" $ toStringP res
