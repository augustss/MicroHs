import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Environment(getArgs)
import MicroHs.Parse
import MicroHs.Exp hiding (compile)
import MicroHs.Compile
import MicroHs.Translate
import MicroHs.Desugar(LDef)
import Compat

main :: IO ()
main = do
  args <- getArgs
  let mn = case filter ((/= "-") . take 1) args of
             [s] -> s
             _ -> error "Usage: uhs [-v] [-iPATH] ModuleName"
      flags = Flags { verbose = "-v" `elem` args,
                      runIt   = "-r" `elem` args,
                      paths   = "." : catMaybes (map (stripPrefix "-i") args)
                    }
  cmdl@(mainName, ds) <- compileTop flags mn
  let defs :: M.Map Ident Exp
      defs = M.fromList [ (n, ref i) | ((n, _), i) <- zip ds [0..] ]
      findIdent n = fromMaybe (error $ "undefined: " ++ show n) $ M.lookup n defs
      emain = findIdent mainName
      subst (Var n) = findIdent n
      subst (App f a) = App (subst f) (subst a)
      subst e = e
      def :: ((Ident, Exp), Int) -> String -> String
      def ((_, e), i) r = "(($T :" ++ showInt i ++ " " ++ toStringP (subst e) ++ ") " ++ r ++ ")"
        -- App2 CT (Lbl i (subst e)) r
      ref i = Var $ "_" ++ showInt i
      res = foldr def (toStringP emain) (zip ds (enumFrom 0))
  when (verbose flags) $ do
    mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toStringP e) ds
    --putStrLn $ toStringP res
  if runIt flags then do
    let prg = translate cmdl
    putStrLn "Run:"
    prg
    putStrLn "done"
   else
    writeFile "out.comb" res

type CModule = (Ident, [LDef])

compileTop :: Flags -> IdentModule -> IO CModule
compileTop flags mn = do
  ds <- compile flags mn
  let ds' = [ (n, compileOpt e) | (n, e) <- ds ]
  return (qual mn "main", ds')
