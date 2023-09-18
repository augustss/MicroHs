-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module MicroHs.Main(main) where
import Prelude
import qualified MicroHs.StringMapFast as M
import Data.Maybe
import System.Environment
import MicroHs.Compile
import MicroHs.Desugar
import MicroHs.Expr
import MicroHs.Exp
import MicroHs.Ident
import MicroHs.Translate
--Ximport Compat

main :: IO ()
main = do
  aargs <- getArgs
  let
    args = takeWhile (not . eqString "--") aargs
    mn =
      let
        ss = filter (not . (eqString "-") . take 1) args
      in   if length ss == 1 then head ss else error "Usage: mhs [-v] [-r] [-iPATH] [-oFILE] ModuleName"
    flags = Flags (length (filter (eqString "-v") args))
                  (elemBy eqString "-r" args)
                  ("." : catMaybes (map (stripPrefixBy eqChar "-i") args))
                  (head $ catMaybes (map (stripPrefixBy eqChar "-o") args) ++ ["out.comb"])
  cmdl <- compileTop flags (mkIdent mn)
  t1 <- getTimeMilli
  let
    (mainName, ds) = cmdl
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
    --def :: ((Ident, Exp), Int) -> String -> String
    def d r =
      case d of
        ((_, e), i) -> "(($A :" ++ showInt i ++ " " ++ toStringP (substv e) ++ ") " ++ r ++ ")"
        -- App2 CT (Lbl i (subst e)) r
    res = foldr def (toStringP emain) (zip ds (enumFrom 0))
    numDefs = M.size defs
  when (verbose flags > 0) $
    putStrLn $ "top level defns: " ++ showInt numDefs
  when (verbose flags > 1) $
    mapM_ (\ (i, e) -> putStrLn $ showIdent i ++ " = " ++ toStringP e) ds
  if runIt flags then do
    let
      prg = translate cmdl
--    putStrLn "Run:"
    prg
--    putStrLn "done"
   else do
    writeFile (output flags) $ version ++ showInt numDefs ++ "\n" ++ res
    t2 <- getTimeMilli
    when (verbose flags > 0) $
      putStrLn $ "final pass            " ++ padLeft 6 (showInt (t2-t1)) ++ "ms"

version :: String
version = "v3.4\n"

type Program = (Ident, [LDef])

compileTop :: Flags -> IdentModule -> IO Program
compileTop flags mn = do
  ds <- compile flags mn
  t1 <- getTimeMilli
  let
    dsn = [ (n, compileOpt e) | (n, e) <- ds ]
  putStr $ drop 1000000 $ showLDefs dsn
  t2 <- getTimeMilli
  when (verbose flags > 0) $
    putStrLn $ "combinator conversion " ++ padLeft 6 (showInt (t2-t1)) ++ "ms"
  return (qualIdent mn (mkIdent "main"), dsn)
