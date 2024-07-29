-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-unused-imports #-}
module MicroHs.Main(main) where
import Prelude
import Data.Char
import Data.List
import Control.DeepSeq
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Version
import System.Environment
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.ExpPrint
import MicroHs.FFI
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.List
import MicroHs.Package
import MicroHs.Translate
import MicroHs.TypeCheck(tModuleName)
import MicroHs.Interactive
import MicroHs.MakeCArray
import System.Cmd
import System.Exit
import System.Directory
import System.IO
import System.IO.Serialize
import System.IO.TimeMilli
import Compat
import MicroHs.Instances() -- for GHC
import MicroHs.TargetConfig
import Paths_MicroHs(version, getDataDir)

mhsVersion :: String
mhsVersion = showVersion version

main :: IO ()
main = do
  args <- getArgs
  dir <- getMhsDir
  dataDir <- getDataDir
  case args of
   ["--version"] -> putStrLn $ "MicroHs, version " ++ mhsVersion ++ ", combinator file version " ++ combVersion
   ["--numeric-version"] -> putStrLn mhsVersion
   _ -> do
    let dflags = (defaultFlags dir){ pkgPath = pkgPaths }
        (flags, mdls, rargs) = decodeArgs dflags [] args
        pkgPaths | dir == dataDir && dir /= "." = [dataDir ++ "/../.."]  -- This is a bit ugly
                 | otherwise                    = []                     -- No package search path
    case listPkg flags of
      Just p -> mainListPkg flags p
      Nothing ->
        case buildPkg flags of
          Just p -> mainBuildPkg flags p mdls
          Nothing ->
            if installPkg flags then mainInstallPackage flags mdls else
            withArgs rargs $ do
              when (verbosityGT flags 0) $
                putStrLn $ "flags = " ++ show flags
              case mdls of
                []  -> mainInteractive flags
                [s] -> mainCompile flags (mkIdentSLoc (SLoc "command-line" 0 0) s)
                _   -> error usage

usage :: String
usage = "Usage: mhs [--version] [--numeric-version] [-v] [-q] [-l] [-r] [-C[R|W]] [-XCPP] [-Ddef] [-T] [-z] [-iPATH] [-oFILE] [-a[PATH]] [-LPATH] [-PPKG] [-Q PKG] [-tTARGET] [ModuleName...]"

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
    '-':'i':[]  -> decodeArgs f{paths = []} mdls args
    '-':'i':s   -> decodeArgs f{paths = paths f ++ [s]} mdls args
    '-':'o':s   -> decodeArgs f{output = s} mdls args
    '-':'t':s   -> decodeArgs f{target = s} mdls args
    '-':'D':_   -> decodeArgs f{cppArgs = cppArgs f ++ [arg]} mdls args
    '-':'I':_   -> decodeArgs f{cppArgs = cppArgs f ++ [arg]} mdls args
    '-':'P':s   -> decodeArgs f{buildPkg = Just s} mdls args
    '-':'a':[]  -> decodeArgs f{pkgPath = []} mdls args
    '-':'a':s   -> decodeArgs f{pkgPath = s : pkgPath f} mdls args
    '-':'L':s   -> decodeArgs f{listPkg = Just s} mdls args
    '-':_       -> error $ "Unknown flag: " ++ arg ++ "\n" ++ usage
    _           -> decodeArgs f (mdls ++ [arg]) args


readTargets :: Flags -> FilePath -> IO [Target]
readTargets flags dir = do
  let tgFilePath = dir ++ "/targets.conf"
  exists <- doesFileExist tgFilePath
  if not exists 
     then return []
     else do
       tgFile <- readFile tgFilePath
       case parseTargets tgFilePath tgFile of
         Left e -> do
           putStrLn $ "Can't read " ++ tgFilePath
           when (verbose flags > 0) $
             putStrLn e
           return []
         Right tgs -> do
           when (verbose flags > 0) $
             putStrLn $ "Read targets file. Possible targets: " ++ show 
               [tg | Target tg _ <- tgs]
           return tgs

readTarget :: Flags -> FilePath -> IO TTarget
readTarget flags dir = do
  targets <- readTargets flags dir
  compiler <- lookupEnv "CC"
  conf <- lookupEnv "MHSCONF"
  let dConf = "unix-" ++ show _wordSize
  case findTarget (target flags) targets of
    Nothing -> do
      when (verbose flags > 0) $
        putStrLn $ unwords ["Could not find", target flags, "in file"]
      return TTarget { tName = "default"
                     , tCC   = fromMaybe "cc" compiler 
                     , tConf = fromMaybe dConf conf
                     }
    Just (Target n cs) -> do
      when (verbose flags > 0) $
        putStrLn $ "Found target: " ++ show cs
      return TTarget { tName = n
                     , tCC   = fromMaybe "cc"  $ compiler <|> lookup "cc"   cs
                     , tConf = fromMaybe dConf $ conf     <|> lookup "conf" cs
                     }


mainBuildPkg :: Flags -> String -> [String] -> IO ()
mainBuildPkg flags namever amns = do
  when (verbose flags > 0) $
    putStrLn $ "Building package " ++ namever
  let mns = map mkIdent amns
  cash <- compileMany flags mns emptyCache
  let mdls = getCompMdls cash
      (name, ver) = splitNameVer namever
      (exported, other) = partition ((`elem` mns) . tModuleName) mdls
      pkgDeps = map (\ p -> (pkgName p, pkgVersion p)) $ getPkgs cash
      pkg = Package { pkgName = mkIdent name, pkgVersion = ver
                    , pkgExported = exported, pkgOther = other
                    , pkgDepends = pkgDeps }
  --print (map tModuleName $ pkgOther pkg)
  when (verbose flags > 0) $
    putStrLn $ "Writing package " ++ namever ++ " to " ++ output flags
  writeSerializedCompressed (output flags) pkg

splitNameVer :: String -> (String, Version)
splitNameVer s =
  case span (\ c -> isDigit c || c == '.') (reverse s) of
    (rver, '-':rname) | is@(_:_) <- readVersion (reverse rver) -> (reverse rname, makeVersion is)
    _ -> error $ "package name not of the form name-version:" ++ show s
  where readVersion = map read . words . map (\ c -> if c == '.' then ' ' else c)

mainListPkg :: Flags -> FilePath -> IO ()
mainListPkg _flags pkgfn = do
  pkg <- readSerialized pkgfn
  let list = mapM_ (putStrLn . showIdent . tModuleName)
  list (pkgExported pkg)
  list (pkgOther pkg)

mainCompile :: Flags -> Ident -> IO ()
mainCompile flags mn = do
  (rmn, allDefs) <- do
    cash <- getCached flags
    (rds, _, cash') <- compileCacheTop flags mn cash
    maybeSaveCache flags cash'
    return rds

  t1 <- getTimeMilli
  let
    mainName = qualIdent rmn (mkIdent "main")
    cmdl = (mainName, allDefs)
    outData = toStringCMdl cmdl
    numDefs = length allDefs
  when (verbosityGT flags 0) $
    putStrLn $ "top level defns: " ++ show numDefs
  when (verbosityGT flags 2) $
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
       let dir = mhsdir flags
       TTarget _ compiler conf <- readTarget flags dir
       let cc = fromMaybe (compiler ++ " -w -Wall -O3 -I" ++ dir ++ "/src/runtime " ++ dir ++ "/src/runtime/eval-" ++ conf ++ ".c " ++ " $IN -lm -o $OUT") mcc
           cmd = substString "$IN" fn $ substString "$OUT" outFile cc
       when (verbosityGT flags 0) $
         putStrLn $ "Execute: " ++ show cmd
       ec <- system cmd
       removeFile fn
       when (ec /= ExitSuccess) $
         error $ "command failed: " ++ cmd
       ct2 <- getTimeMilli
       when (verbosityGT flags 0) $
         putStrLn $ "C compilation         " ++ padLeft 6 (show (ct2-ct1)) ++ "ms"

mainInstallPackage :: Flags -> [FilePath] -> IO ()
mainInstallPackage flags [pkgfn, dir] = do
  when (verbosityGT flags (-1)) $
    putStrLn $ "Installing package " ++ pkgfn ++ " in " ++ dir
  pkg <- readSerialized pkgfn
  let pdir = dir ++ "/" ++ packageDir
      pkgout = unIdent (pkgName pkg) ++ "-" ++ showVersion (pkgVersion pkg) ++ packageSuffix
  createDirectoryIfMissing True pdir
  copyFile pkgfn (pdir ++ "/" ++ pkgout)
  let mk tm = do
        let fn = dir ++ "/" ++ moduleToFile (tModuleName tm) ++ packageTxtSuffix
            d = dropWhileEnd (/= '/') fn
        when (verbosityGT flags 2) $
          putStrLn $ "create " ++ fn
        createDirectoryIfMissing True d
        writeFile fn pkgout
  mapM_ mk (pkgExported pkg)
mainInstallPackage flags [pkgfn] = mainInstallPackage flags [pkgfn, head (pkgPath flags)]
mainInstallPackage _ _ = error usage

-----------------

getMhsDir :: IO FilePath
getMhsDir = do
  md <- lookupEnv "MHSDIR"
  case md of
    Just d -> return d
    Nothing -> getDataDir
