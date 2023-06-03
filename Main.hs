import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Environment(getArgs)
import Parse
import Exp hiding (compile)
import Compile

main :: IO ()
main = do
  args <- getArgs
  let mn = case filter ((/= "-") . take 1) args of
             [s] -> s
             _ -> error "Usage: uhs [-v] [-iPATH] ModuleName"
      flags = Flags { verbose = "-v" `elem` args,
                      path = "." : catMaybes (map (stripPrefix "-i") args)
                    }
  ds <- compile flags mn
  let ds' = [ (n, compileOpt e) | (n, e) <- ds ]
      defs :: M.Map Ident Exp
      defs = M.fromList [ (n, ref i) | ((n, _), i) <- zip ds' [0..] ]
      findIdent n = fromMaybe (error $ "undefined: " ++ show n) $ M.lookup n defs
      emain = findIdent $ qual mn "main"
      res = foldr def emain (zip ds' [0..])
      def :: ((Ident, Exp), Int) -> Exp -> Exp
      def ((_, e), i) r = App2 CT (Lbl i (subst e)) r
      subst (Var n) = findIdent n
      subst (App f a) = App (subst f) (subst a)
      subst e = e
      ref i = Var $ "_" ++ show (i::Int)
  when (verbose flags) $ do
    mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toStringP e) ds'
    --putStrLn $ toStringP res
  writeFile "out.comb" $ toStringP res
