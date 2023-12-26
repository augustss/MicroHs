-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module MicroHs.Main(main) where
import Prelude
import Data.List
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import System.Environment
import MicroHs.Compile
import MicroHs.Exp
import MicroHs.ExpPrint
import MicroHs.Flags
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.Translate
import MicroHs.Interactive
import MicroHs.MakeCArray
import System.Directory
import System.IO
import System.Process
import Compat
import MicroHs.Instances() -- for GHC

-- Version number of combinator file.
-- Must match version in eval.c.
combVersion :: String
combVersion = "v6.1\n"

mhsVersion :: String
mhsVersion = "0.9.2.0"

main :: IO ()
main = do
  aargs <- getArgs
  mdir <- lookupEnv "MHSDIR"
  let dir = fromMaybe "." mdir
  let
    args = takeWhile (/= "--") aargs
    ss = filter ((/= "-") . take 1) args
    flags = Flags {
      verbose  = length (filter (== "-v") args),
      runIt    = elem "-r" args,
      paths    = "." : (dir ++ "/lib") : catMaybes (map (stripPrefix "-i") args),
      output   = head $ catMaybes (map (stripPrefix "-o") args) ++ ["out.comb"],
      loading  = elem "-l" args,
      useCache = elem "-C" args && usingMhs,
      useTicks = elem "-T" args
      }
  if "--version" `elem` args then
    putStrLn $ "MicroHs, version " ++ mhsVersion ++ ", combinator file version " ++ combVersion
   else
    case ss of
      []  -> mainInteractive flags
      [s] -> mainCompile dir flags (mkIdentSLoc (SLoc "command-line" 0 0) s)
      _   -> error "Usage: mhs [-v] [-l] [-r] [-C] [-T] [-iPATH] [-oFILE] [ModuleName]"

mainCompile :: FilePath -> Flags -> Ident -> IO ()
mainCompile mhsdir flags mn = do
  ds <- if flags.useCache then do
          cash <- getCached flags
          (ds, cash') <- compileCacheTop flags mn cash
          when (verbosityGT flags 0) $
            putStrLn $ "Saving cache " ++ show mhsCacheName
          () <- seq (rnf cash') (return ())
          hout <- openFile mhsCacheName WriteMode
          hSerialize hout cash'
          hClose hout
          return ds
        else
          fst <$> compileCacheTop flags mn emptyCache

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
  when (verbosityGT flags 0) $
    putStrLn $ "top level defns: " ++ show numDefs
  when (verbosityGT flags 1) $
    mapM_ (\ (i, e) -> putStrLn $ showIdent i ++ " = " ++ toStringP e "") ds
  if flags.runIt then do
    let
      prg = translateAndRun cmdl
--    putStrLn "Run:"
--    writeSerialized "ser.comb" prg
    prg
--    putStrLn "done"
   else do
    let outData = combVersion ++ show numDefs ++ "\n" ++ res
    seq (length outData) (return ())
    t2 <- getTimeMilli
    when (verbosityGT flags 0) $
      putStrLn $ "final pass            " ++ padLeft 6 (show (t2-t1)) ++ "ms"

    -- Decode what to do:
    --  * file ends in .comb: write combinator file
    --  * file ends in .c: write C version of combinator
    --  * otherwise, write C file and compile to a binary with cc
    let outFile = flags.output
    if ".comb" `isSuffixOf` outFile then
      writeFile outFile outData
     else if ".c" `isSuffixOf` outFile then
      writeFile outFile $ makeCArray outData
     else do
       (fn, h) <- openTmpFile "mhsc.c"
       hPutStr h $ makeCArray outData
       hClose h
       ct1 <- getTimeMilli
       mcc <- lookupEnv "MHSCC"
       compiler <- fromMaybe "cc" <$> lookupEnv "CC"
       let conf = "unix-" ++ show _wordSize
           cc = fromMaybe (compiler ++ " -w -Wall -O3 " ++ mhsdir ++ "/src/runtime/eval-" ++ conf ++ ".c " ++ " $IN -lm -o $OUT") mcc
           cmd = substString "$IN" fn $ substString "$OUT" outFile cc
       when (verbosityGT flags 0) $
         putStrLn $ "Execute: " ++ show cmd
       callCommand cmd
       removeFile fn
       ct2 <- getTimeMilli
       when (verbosityGT flags 0) $
         putStrLn $ "C compilation         " ++ padLeft 6 (show (ct2-ct1)) ++ "ms"
