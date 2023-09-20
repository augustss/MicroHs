-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module MicroHs.Main(main) where
import Prelude
import qualified MicroHs.StringMapFast as M
import Data.Maybe
import System.Environment
import MicroHs.Compile
import MicroHs.Exp
import MicroHs.Ident
import MicroHs.Translate
import MicroHs.Interactive
--Ximport Compat

main :: IO ()
main = do
  aargs <- getArgs
  let
    args = takeWhile (not . eqString "--") aargs
    ss = filter (not . (eqString "-") . take 1) args
    flags = Flags (length (filter (eqString "-v") args))
                  (elemBy eqString "-r" args)
                  ("." : catMaybes (map (stripPrefixBy eqChar "-i") args))
                  (head $ catMaybes (map (stripPrefixBy eqChar "-o") args) ++ ["out.comb"])
  case ss of
    [] -> mainInteractive flags
    [s] -> mainCompile flags (mkIdent s)
    _ -> error "Usage: mhs [-v] [-r] [-iPATH] [-oFILE] [ModuleName]"

mainCompile :: Flags -> Ident -> IO ()
mainCompile flags mn = do
  ds <- compileTop flags mn
  t1 <- getTimeMilli
  let
    mainName = qualIdent mn (mkIdent "main")
    cmdl = (mainName, ds)
    ref i = Var $ mkIdent $ "_" ++ showInt i
    defs = M.fromList [ (unIdent n, ref i) | ((n, _), i) <- zip ds (enumFrom 0) ]
    findIdent n = fromMaybe (error $ "main: findIdent: " ++ showIdent n) $
                  M.lookup (unIdent n) defs
    emain = findIdent mainName
    substv aexp =
      case aexp of
        Var n -> findIdent n
        App f a -> App (substv f) (substv a)
        e -> e
    def :: ((Ident, Exp), Int) -> (String -> String) -> (String -> String)
    def ((_, e), i) r =
      (("(($A :" ++ showInt i ++ " ") ++) . toStringP (substv e) . (") " ++) . r . (")" ++)
    res = foldr def (toStringP emain) (zip ds (enumFrom 0)) ""
    numDefs = M.size defs
  when (verbose flags > 0) $
    putStrLn $ "top level defns: " ++ showInt numDefs
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
    writeFile (output flags) $ version ++ showInt numDefs ++ "\n" ++ res
    t2 <- getTimeMilli
    when (verbose flags > 0) $
      putStrLn $ "final pass            " ++ padLeft 6 (showInt (t2-t1)) ++ "ms"

version :: String
version = "v3.4\n"
