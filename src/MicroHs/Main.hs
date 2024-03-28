-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-unused-imports #-}
module MicroHs.Main(main) where
import Prelude
import Data.List
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import Data.Version
import System.Environment
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.ExpPrint
import MicroHs.FFI
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.Package
import MicroHs.Translate
import MicroHs.TypeCheck(tModuleName)
import MicroHs.Interactive
import MicroHs.MakeCArray
import System.Directory
import System.IO
import System.IO.Serialize
import System.Process
import Compat
import MicroHs.Instances(getMhsDir) -- for GHC

mhsVersion :: String
mhsVersion = "0.9.9.0"

main :: IO ()
main = do
  args <- getArgs
  dir <- fromMaybe "." <$> getMhsDir
  home <- getEnv "HOME"
  case args of
   ["--version"] -> putStrLn $ "MicroHs, version " ++ mhsVersion ++ ", combinator file version " ++ combVersion
   ["--numeric-version"] -> putStrLn mhsVersion
   _ -> do
    let dflags = (defaultFlags dir){ pkgPath = [home ++ "/.mcabal/mhs-" ++ mhsVersion] }
        (flags, mdls, rargs) = decodeArgs dflags [] args
    case buildPkg flags of
      Just p -> mainBuildPkg flags p mdls
      Nothing ->
        if installPkg flags then mainInstallPackage flags mdls else
        withArgs rargs $
          case mdls of
            []  -> mainInteractive flags
            [s] -> mainCompile flags (mkIdentSLoc (SLoc "command-line" 0 0) s)
            _   -> error usage

usage :: String
usage = "Usage: mhs [--version] [--numeric-version] [-v] [-q] [-l] [-r] [-C[R|W]] [-XCPP] [-Ddef] [-T] [-z] [-iPATH] [-oFILE] [-PPKG] [-Q PKG] [ModuleName...]"

decodeArgs :: Flags -> [String] -> [String] -> (Flags, [String], [String])
decodeArgs f mdls [] = (f, mdls, [])
decodeArgs f mdls (arg:args) =
  case arg of
    "--"        -> (f, mdls, args)              -- leave arguments after -- for any program we run
    "-v"        -> decodeArgs f{verbose = verbose f + 1} mdls args
    "-q"        -> decodeArgs f{verbose = -1} mdls args
    "-r"        -> decodeArgs f{runIt = True} mdls args
    "-l"        -> decodeArgs f{loading = True} mdls args
    "-CR"       -> decodeArgs f{readCache = True} mdls args
    "-CW"       -> decodeArgs f{writeCache = True} mdls args
    "-C"        -> decodeArgs f{readCache=True, writeCache = True} mdls args
    "-T"        -> decodeArgs f{useTicks = True} mdls args
    "-XCPP"     -> decodeArgs f{doCPP = True} mdls args
    "-z"        -> decodeArgs f{compress = True} mdls args
    "-Q"        -> decodeArgs f{installPkg = True} mdls args
    '-':'i':s   -> decodeArgs f{paths = paths f ++ [s]} mdls args
    '-':'o':s   -> decodeArgs f{output = s} mdls args
    '-':'D':_   -> decodeArgs f{cppArgs = cppArgs f ++ [arg]} mdls args
    '-':'I':_   -> decodeArgs f{cppArgs = cppArgs f ++ [arg]} mdls args
    '-':'P':s   -> decodeArgs f{buildPkg = Just s} mdls args
    '-':_       -> error $ "Unknown flag: " ++ arg ++ "\n" ++ usage
    _           -> decodeArgs f (mdls ++ [arg]) args

mainBuildPkg :: Flags -> String -> [String] -> IO ()
mainBuildPkg flags pkgn amns = do
  when (verbose flags > 0) $
    putStrLn $ "Building package " ++ pkgn
  let mns = map mkIdent amns
  cash <- compileMany flags mns emptyCache
  let mdls = getCompMdls cash
      (exported, other) = partition ((`elem` mns) . tModuleName) mdls
      pkgDeps = map pkgName $ getPkgs cash
      pkg = Package { pkgName = mkIdent pkgn, pkgVersion = makeVersion [0]
                    , pkgExported = exported, pkgOther = other
                    , pkgDepends = map (,makeVersion [0]) pkgDeps }
  print (map tModuleName $ pkgOther pkg)
  when (verbose flags > 0) $
    putStrLn $ "Writing package " ++ pkgn ++ " to " ++ output flags
  writeSerializedCompressed (output flags) pkg

mainCompile :: Flags -> Ident -> IO ()
mainCompile flags mn = do
  (rmn, allDefs) <-
    if writeCache flags then do
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
  if runIt flags then do
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
    let outFile = output flags
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
       let dir = mhsdir flags
           conf = "unix-" ++ show _wordSize
           cc = fromMaybe (compiler ++ " -w -Wall -O3 -I" ++ dir ++ "/src/runtime " ++ dir ++ "/src/runtime/eval-" ++ conf ++ ".c " ++ " $IN -lm -o $OUT") mcc
           cmd = substString "$IN" fn $ substString "$OUT" outFile cc
       when (verbosityGT flags 0) $
         putStrLn $ "Execute: " ++ show cmd
       callCommand cmd
       removeFile fn
       ct2 <- getTimeMilli
       when (verbosityGT flags 0) $
         putStrLn $ "C compilation         " ++ padLeft 6 (show (ct2-ct1)) ++ "ms"

mainInstallPackage :: Flags -> [FilePath] -> IO ()
mainInstallPackage flags [pkgfn, dir] = do
  when (verbosityGT flags (-1)) $
    putStrLn $ "Installing package " ++ pkgfn ++ " in " ++ dir
  pkg <- readSerialized pkgfn
  let pdir = dir ++ "/" ++ packageDir
      pkgfn' = dropSuffix pkgfn ++ packageSuffix
  createDirectoryIfMissing True pdir
  copyFile pkgfn (pdir ++ "/" ++ pkgfn')
  let mk tm = do
        let fn = dir ++ "/" ++ moduleToFile (tModuleName tm)
            d = dropWhileEnd (/= '/') fn
        when (verbosityGT flags 1) $
          putStrLn $ "create " ++ fn
        createDirectoryIfMissing True d
        writeFile fn pkgfn
  mapM_ mk (pkgExported pkg)
mainInstallPackage flags [pkgfn] = mainInstallPackage flags [pkgfn, head (pkgPath flags)]
mainInstallPackage _ _ = error usage

dropSuffix :: FilePath -> FilePath
dropSuffix s =
  case dropWhileEnd (/= '.') s of
    [] -> s
    r  -> r
