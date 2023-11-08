-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module MicroHs.Main(main) where
import Prelude
--Ximport Data.List
import Control.Monad
import Data.Maybe
import System.Environment
import MicroHs.Compile
import MicroHs.Exp
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.Translate
import MicroHs.Interactive
import MicroHs.MakeCArray
--Ximport Compat

main :: IO ()
main = do
  aargs <- getArgs
  let
    args = takeWhile (/= "--") aargs
    ss = filter ((/= "-") . take 1) args
    flags = Flags (length (filter (== "-v") args))
                  (elem "-r" args)
                  ("." : catMaybes (map (stripPrefix "-i") args))
                  (head $ catMaybes (map (stripPrefix "-o") args) ++ ["out.comb"])
                  (elem "-l" args)
  case ss of
    [] -> mainInteractive flags
    [s] -> mainCompile flags (mkIdentSLoc (SLoc "command-line" 0 0) s)
    _ -> error "Usage: mhs [-v] [-r] [-iPATH] [-oFILE] [ModuleName]"

mainCompile :: Flags -> Ident -> IO ()
mainCompile flags mn = do
  ds <- compileTop flags mn
  t1 <- getTimeMilli
  let
    mainName = qualIdent mn (mkIdent "main")
    cmdl = (mainName, ds)
    ref i = Var $ mkIdent $ "_" ++ show i
    defs = M.fromList [ (n, ref i) | ((n, _), i) <- zip ds (enumFrom (0::Int)) ]
    findIdent n = fromMaybe (error $ "main: findIdent: " ++ showIdent n) $
                  M.lookup n defs
    emain = findIdent mainName
    substv aexp =
      case aexp of
        Var n -> findIdent n
        App f a -> App (substv f) (substv a)
        e -> e
    def :: ((Ident, Exp), Int) -> (String -> String) -> (String -> String)
    def ((_, e), i) r =
      (("((A :" ++ show i ++ " ") ++) . toStringP (substv e) . (") " ++) . r . (")" ++)
    res = foldr def (toStringP emain) (zip ds (enumFrom 0)) ""
    numDefs = M.size defs
  when (verbose flags > 0) $
    putStrLn $ "top level defns: " ++ show numDefs
  when (verbose flags > 1) $
    mapM_ (\ (i, e) -> putStrLn $ showIdent i ++ " = " ++ toStringP e "") ds
  if runIt flags then do
    let
      prg = translateAndRun cmdl
--    putStrLn "Run:"
--    writeSerialized "ser.comb" prg
    prg
--    putStrLn "done"
   else do
    let outFile = output flags
        outData = version ++ show numDefs ++ "\n" ++ res
        outData' = if ".c" `isSuffixOf` outFile then makeCArray outData else outData
    writeFile outFile outData'
    t2 <- getTimeMilli
    when (verbose flags > 0) $
      putStrLn $ "final pass            " ++ padLeft 6 (show (t2-t1)) ++ "ms"

version :: String
version = "v4.2\n"
