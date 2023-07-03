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
      res = foldr def emain (zip ds [0..])
      def :: ((Ident, Exp), Int) -> Exp -> Exp
      def ((_, e), i) r = App2 CT (Lbl i (subst e)) r
      subst (Var n) = findIdent n
      subst (App f a) = App (subst f) (subst a)
      subst e = e
      ref i = Var $ "_" ++ show (i::Int)
  when (verbose flags) $ do
    mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toStringP e) ds
    --putStrLn $ toStringP res
  if runIt flags then do
    let prg = translate cmdl
    putStrLn "Run:"
    prg
    putStrLn "done"
   else
    writeFile "out.comb" $ toStringP res

type CModule = (Ident, [LDef])

compileTop :: Flags -> IdentModule -> IO CModule
compileTop flags mn = do
  ds <- compile flags mn
  let ds' = [ (n, compileOpt e) | (n, e) <- ds ]
  return (qual mn "main", ds')
