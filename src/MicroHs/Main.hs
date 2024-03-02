-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-unused-imports #-}
module MicroHs.Main(main) where
import Prelude
import Data.List
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import System.Environment
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.ExpPrint
import MicroHs.FFI
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.Translate
import MicroHs.Interactive
import MicroHs.MakeCArray
import System.Directory
import System.IO
import System.Process
import Compat
import MicroHs.Instances(getMhsDir) -- for GHC

mhsVersion :: String
mhsVersion = "0.9.7.0"

main :: IO ()
main = do
  args <- getArgs
  dir <- fromMaybe "." <$> getMhsDir
  if take 1 args == ["--version"] then
    putStrLn $ "MicroHs, version " ++ mhsVersion ++ ", combinator file version " ++ combVersion
   else do
    let (flags, mdls, rargs) = decodeArgs (defaultFlags dir) [] args
    withArgs rargs $              -- leave arguments after -- for any program we run
      case mdls of
        []  -> mainInteractive flags
        [s] -> mainCompile flags (mkIdentSLoc (SLoc "command-line" 0 0) s)
        _   -> error usage

usage :: String
usage = "Usage: mhs [--version] [-v] [-l] [-r] [-C[R|W]] [-XCPP] [-Ddef] [-T] [-z] [-iPATH] [-oFILE] [ModuleName]"

decodeArgs :: Flags -> [String] -> [String] -> (Flags, [String], [String])
decodeArgs f mdls [] = (f, mdls, [])
decodeArgs f mdls (arg:args) =
  case arg of
    "--"        -> (f, mdls, args)
    "-v"        -> decodeArgs f{verbose = f.verbose + 1} mdls args
    "-r"        -> decodeArgs f{runIt = True} mdls args
    "-l"        -> decodeArgs f{loading = True} mdls args
    "-CR"       -> decodeArgs f{readCache = True} mdls args
    "-CW"       -> decodeArgs f{writeCache = True} mdls args
    "-C"        -> decodeArgs f{readCache=True, writeCache = True} mdls args
    "-T"        -> decodeArgs f{useTicks = True} mdls args
    "-XCPP"     -> decodeArgs f{doCPP = True} mdls args
    "-z"        -> decodeArgs f{compress = True} mdls args
    '-':'i':s   -> decodeArgs f{paths = f.paths ++ [s]} mdls args
    '-':'o':s   -> decodeArgs f{output = s} mdls args
    '-':'D':s   -> decodeArgs f{cppArgs = f.cppArgs ++ [s]} mdls args
    '-':_       -> error $ "Unknown flag: " ++ arg ++ "\n" ++ usage
    _           -> decodeArgs f (mdls ++ [arg]) args

mainCompile :: Flags -> Ident -> IO ()
mainCompile flags mn = do
  (rmn, allDefs) <-
    if flags.writeCache then do
      cash <- getCached flags
      (rds, cash') <- compileCacheTop flags mn cash
      when (verbosityGT flags 0) $
        putStrLn $ "Saving cache " ++ show mhsCacheName
      () <- seq (rnfNoErr cash) (return ())
      saveCache mhsCacheName cash'
      return rds
    else do
      cash <- getCached flags
      fst <$> compileCacheTop flags mn cash

  t1 <- getTimeMilli
  let
    mainName = qualIdent rmn (mkIdent "main")
    cmdl = (mainName, allDefs)
    outData = toStringCMdl cmdl
    numDefs = length allDefs
  when (verbosityGT flags 0) $
    putStrLn $ "top level defns: " ++ show numDefs
  when (verbosityGT flags 1) $
    mapM_ (\ (i, e) -> putStrLn $ showIdent i ++ " = " ++ toStringP e "") allDefs
  if flags.runIt then do
    let
      prg = translateAndRun cmdl
--    putStrLn "Run:"
--    writeSerialized "ser.comb" prg
    prg
--    putStrLn "done"
   else do
    seq (length outData) (return ())
    t2 <- getTimeMilli
    when (verbosityGT flags 0) $
      putStrLn $ "final pass            " ++ padLeft 6 (show (t2-t1)) ++ "ms"

    let cCode = makeCArray flags outData ++ makeFFI flags allDefs

    -- Decode what to do:
    --  * file ends in .comb: write combinator file
    --  * file ends in .c: write C version of combinator
    --  * otherwise, write C file and compile to a binary with cc
    let outFile = flags.output
    if ".comb" `isSuffixOf` outFile then
      writeFile outFile outData
     else if ".c" `isSuffixOf` outFile then
      writeFile outFile cCode
     else do
       (fn, h) <- openTmpFile "mhsc.c"
       hPutStr h cCode
       hClose h
       ct1 <- getTimeMilli
       mcc <- lookupEnv "MHSCC"
       compiler <- fromMaybe "cc" <$> lookupEnv "CC"
       let conf = "unix-" ++ show _wordSize
           cc = fromMaybe (compiler ++ " -w -Wall -O3 -I" ++ flags.mhsdir ++ "/src/runtime " ++ flags.mhsdir ++ "/src/runtime/eval-" ++ conf ++ ".c " ++ " $IN -lm -o $OUT") mcc
           cmd = substString "$IN" fn $ substString "$OUT" outFile cc
       when (verbosityGT flags 0) $
         putStrLn $ "Execute: " ++ show cmd
       callCommand cmd
       removeFile fn
       ct2 <- getTimeMilli
       when (verbosityGT flags 0) $
         putStrLn $ "C compilation         " ++ padLeft 6 (show (ct2-ct1)) ++ "ms"
