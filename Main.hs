import Control.Monad
import Data.Maybe
import System.Environment(getArgs)
import Parse
import Exp hiding (compile)
import Compile

main :: IO ()
main = do
  args <- getArgs
  let verbose = "-v" `elem` args
      mn = case filter ((/= "-") . take 1) args of
             [s] -> s
             _ -> error "Usage: compile [-v] ModuleName"
  ds <- compile verbose mn
  let ds' = [ (n, compileOpt e) | (n, e) <- ds ]
      defs :: [(Ident, Exp)]
      defs = [ (n, ref i) | ((n, _), i) <- zip ds' [0..] ]
      find n = fromMaybe (error $ "undefined: " ++ show n) $ lookup n defs
      emain = find $ mn ++ ".main"
      res = foldr def emain (zip ds' [0..])
      def :: ((Ident, Exp), Int) -> Exp -> Exp
      def ((_, e), i) r = App2 CT (Lbl i (subst e)) r
      subst (Var n) = find n
      subst (App f a) = App (subst f) (subst a)
      subst e = e
      ref i = Var $ "_" ++ show (i::Int)
--  mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toString e) ds
  when verbose $ do
    mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toString e) ds'
    putStrLn $ toStringP res
  writeFile "out.comb" $ toStringP res
