-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module MicroHs.Main(module MicroHs.Main) where
import Prelude
import qualified MicroHs.StringMap as M
import Data.Maybe
import System.Environment
import MicroHs.Parse
import MicroHs.Exp
import MicroHs.Compile
import MicroHs.Translate
import MicroHs.Desugar(LDef)
--Ximport Compat

main :: IO ()
main = do
  args <- getArgs
  let
    mn =
      let
        ss = filter (not . (eqString "-") . take 1) args
      in   if length ss == 1 then head ss else error "Usage: uhs [-v] [-r] [-iPATH] [-oFILE] ModuleName"
    flags = Flags (length (filter (eqString "-v") args))
                  (elemBy eqString "-r" args)
                  ("." : catMaybes (map (stripPrefixBy eqChar "-i") args))
                  (head $ catMaybes (map (stripPrefixBy eqChar "-o") args) ++ ["out.comb"])
  cmdl <- compileTop flags mn
  let
    (mainName, ds) = cmdl
    ref i = Var $ "_" ++ showInt i
    defs = M.fromList [ (n, ref i) | ((n, _), i) <- zip ds (enumFrom 0) ]
    findIdent n = fromMaybe (error $ "undefined: " ++ showIdent n) $ M.lookup n defs
    emain = findIdent mainName
    substv aexp =
      case aexp of
        Var n -> findIdent n
        App f a -> App (substv f) (substv a)
        e -> e
    --def :: ((Ident, Exp), Int) -> String -> String
    def d r =
      case d of
        ((_, e), i) -> "(($T :" ++ showInt i ++ " " ++ toStringP (substv e) ++ ") " ++ r ++ ")"
        -- App2 CT (Lbl i (subst e)) r
    res = foldr def (toStringP emain) (zip ds (enumFrom 0))
  when (verbose flags > 1) $
    mapM_ (\ ie -> case ie of { (i, e) -> putStrLn $ i ++ " = " ++ toStringP e}) ds
  if runIt flags then do
    let
      prg = translate cmdl
    putStrLn "Run:"
    prg
    putStrLn "done"
   else
    writeFile (output flags) $ version ++ res

version :: String
version = "v1.0\n"

type Program = (Ident, [LDef])

compileTop :: Flags -> IdentModule -> IO Program
compileTop flags mn = do
  ds <- compile flags mn
  let
    dsn = [ (n, compileOpt e) | (n, e) <- ds ]
  return (qual mn "main", dsn)
