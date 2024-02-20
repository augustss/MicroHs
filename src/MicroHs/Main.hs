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
mhsVersion = "0.9.5.0"

main :: IO ()
main = do
  aargs <- getArgs
  mdir <- getMhsDir
  let dir = fromMaybe "." mdir
  let
    (args, rargs) = span (/= "--") aargs
    mdls = filter ((/= "-") . take 1) args
    flags = Flags {
      verbose    = length (filter (== "-v") args),
      runIt      = elem "-r" args,
      mhsdir     = dir,
      paths      = "." : (dir ++ "/lib") : catMaybes (map (stripPrefix "-i") args),
      output     = head $ catMaybes (map (stripPrefix "-o") args) ++ ["out.comb"],
      loading    = elem "-l" args,
      readCache  = usingMhs && (elem "-C" args || elem "-CR" args),
      writeCache = usingMhs && (elem "-C" args || elem "-CW" args),
      useTicks   = elem "-T" args
      }
  if "--version" `elem` args then
    putStrLn $ "MicroHs, version " ++ mhsVersion ++ ", combinator file version " ++ combVersion
   else
    withArgs (drop 1 rargs) $              -- leave arguments after -- for any program we run
      case mdls of
        []  -> mainInteractive flags
        [s] -> mainCompile flags (mkIdentSLoc (SLoc "command-line" 0 0) s)
        _   -> error "Usage: mhs [-v] [-l] [-r] [-C] [-T] [-iPATH] [-oFILE] [ModuleName]"

mainCompile :: Flags -> Ident -> IO ()
mainCompile flags mn = do
  ds <- if flags.writeCache then do
          cash <- getCached flags
          (ds, cash') <- compileCacheTop flags mn cash
          when (verbosityGT flags 0) $
            putStrLn $ "Saving cache " ++ show mhsCacheName
          () <- seq (rnfNoErr cash) (return ())
          saveCache mhsCacheName cash'
          return ds
        else do
          cash <- getCached flags
          fst <$> compileCacheTop flags mn cash

  t1 <- getTimeMilli
  let
    mainName = qualIdent mn (mkIdent "main")
    cmdl = (mainName, ds)
    outData = toStringCMdl cmdl
    numDefs = length ds
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
           cc = fromMaybe (compiler ++ " -w -Wall -O3 " ++ flags.mhsdir ++ "/src/runtime/eval-" ++ conf ++ ".c " ++ " $IN -lm -o $OUT") mcc
           cmd = substString "$IN" fn $ substString "$OUT" outFile cc
       when (verbosityGT flags 0) $
         putStrLn $ "Execute: " ++ show cmd
       callCommand cmd
       removeFile fn
       ct2 <- getTimeMilli
       when (verbosityGT flags 0) $
         putStrLn $ "C compilation         " ++ padLeft 6 (show (ct2-ct1)) ++ "ms"
