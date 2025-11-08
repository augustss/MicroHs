-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-unused-imports #-}
module MicroHs.Main(main) where
import qualified Prelude(); import MHSPrelude
import Data.Char
import Data.List
import Data.Version
import Control.Monad
import Control.Applicative
import Data.Maybe
import System.Environment
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.Exp(Exp(Var, Lit))
import MicroHs.Expr(Lit(LInt))
import MicroHs.ExpPrint
import MicroHs.FFI
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.Lex(readInt)
import MicroHs.List
import MicroHs.Package
import MicroHs.Translate
import MicroHs.TypeCheck(TModule(..), showValueExport, showTypeExport, showTypeExportAssocs, TypeExport)
import MicroHs.Interactive
import MicroHs.MakeCArray
import MhsEval
import System.Cmd
import System.Exit
import System.FilePath
import System.Directory
import System.IO
import System.IO.Serialize
import System.IO.TimeMilli
import System.IO.Transducers(addLZ77, addBase64)
import MicroHs.TargetConfig
import Paths_MicroHs(getDataDir)

main :: IO ()
main = do
  args <- getArgs
  dir <- getMhsDir
  dataDir <- getDataDir
  case args of
    ["-h"] -> putStrLn usage
    ["-?"] -> putStrLn usage
    ["--help"] -> putStrLn longUsage
    ["--version"] -> putStrLn $ "MicroHs, version " ++ mhsVersion ++ ", combinator file version " ++ combVersion
    ["--numeric-version"] -> putStrLn mhsVersion
    _ -> do
      let dflags = (defaultFlags dir){ pkgPath = pkgPaths }
          (flags, mdls, rargs) = decodeArgs dflags [] args
          pkgPaths | dir == dataDir && dir /= "." = [takeDirectory $ takeDirectory $ takeDirectory dataDir]   -- This is a bit ugly
                   | otherwise                    = []                        -- No package search path
      when (verbosityGT flags 1) $
        putStrLn $ "flags = " ++ show flags
      case listPkg flags of
        Just p -> mainListPkg flags p
        Nothing -> do
          preload' <- mapM (findAPackage flags) (preload flags)
          let flags' = flags { preload = preload' }
          case buildPkg flags' of
            Just p -> mainBuildPkg flags' p mdls
            Nothing ->
              if installPkg flags' then mainInstallPackage flags' mdls else
              withArgs rargs $ do
                case mdls of
                  []  | null (cArgs flags') -> mainInteractive flags'
                      | otherwise -> mainCompileC flags' [] ""
                  [s] -> mainCompile flags' (mkIdentSLoc (SLoc "command-line" 0 0) s)
                  _   -> mhsError usage

usage :: String
usage = "Usage: mhs [-h|?] [--help] [--version] [--numeric-version] [-v] [-q] [-l] [-s] [-r] [-C[R|W]] [-XCPP] [-DDEF] [-IPATH] [-T] [-z] [-b64] [-iPATH] [-oFILE] [-a[PATH]] [-L[FILE|PKG]] [-PPKG] [-Q PKG [DIR]] [-pFILE] [-tTARGET] [-optc OPTION] [-optl OPTION] [-ddump-PASS] [MODULENAME..|FILE]"

longUsage :: String
longUsage = usage ++ "\nOptions:\n" ++ details
  where
    details = "\
      \-h                 Print usage\n\
      \-?                 Print usage\n\
      \--help             Print this message\n\
      \--version          Print the version\n\
      \--numeric-version  Print the version number\n\
      \-v                 Increase verbosity (flag can be repeated)\n\
      \-q                 Decrease verbosity (flag can be repeated)\n\
      \-l                 Show every time a module is loaded\n\
      \-s                 Show compilation speed in lines/s\n\
      \-r                 Run directly\n\
      \-c                 Do not generate executable\n\
      \-CR                Read compilation cache\n\
      \-CW                Write compilation cache\n\
      \-C                 Read and write compilation cache\n\
      \-XCPP              Run cpphs on source files\n\
      \-Dxxx              Pass -Dxxx to cpphs\n\
      \-Ixxx              Pass -Ixxx to cpphs\n\
      \-T                 Generate dynamic function usage statistics\n\
      \-z                 Compress the combinator code\n\
      \-b64               Base64 encode the combinator code\n\
      \-iPATH             Add PATH to module search path\n\
      \-oFILE             Output to FILE\n\
      \                   If FILE ends in .comb produce a combinator file\n\
      \                   If FILE ends in .c produce a C file\n\
      \                   Otherwise compile the combinators together with the runtime system to produce a regular executable\n\
      \-a                 Clear package search path\n\
      \-aPATH             Add PATH to package search path\n\
      \-L[FILE|PKG]       List all modules of a package\n\
      \-PPKG              Build package PKG\n\
      \-Q PKG [DIR]       Install package PKG\n\
      \-pFILE             Pre-load package\n\
      \-tTARGET           Select target\n\
      \                   Distributed targets: default, emscripten, windows, tcc, environment\n\
      \                   Targets can be defined in targets.conf\n\
      \-optc OPTION       Options for the C compiler\n\
      \-optl OPTION       Options passed by mhs to the C compiler for the linker\n\
      \--stdin            Use stdin in interactive system\n\
      \-ddump-PASS        Debug, print AST after PASS\n\
      \                   Possible passes: preproc, parse, derive, typecheck, desugar, toplevel, combinator, linked, all\n\
      \"

decodeArgs :: Flags -> [String] -> [String] -> (Flags, [String], [String])
decodeArgs f mdls [] = (f, mdls, [])
decodeArgs f mdls (arg:args) =
  case arg of
    "--"        -> (f, mdls, args)              -- leave arguments after -- for any program we run
    "-v"        -> decodeArgs f{verbose = verbose f + 1} mdls args
    "-q"        -> decodeArgs f{verbose = -1} mdls args
    "-r"        -> decodeArgs f{runIt = True} mdls args
    "-l"        -> decodeArgs f{loading = True} mdls args
    "-s"        -> decodeArgs f{speed = True} mdls args
    "-c"        -> decodeArgs f{noLink = True} mdls args
    "-CR"       -> decodeArgs f{readCache = True} mdls args
    "-CW"       -> decodeArgs f{writeCache = True} mdls args
    "-C"        -> decodeArgs f{readCache = True, writeCache = True} mdls args
    "-T"        -> decodeArgs f{useTicks = True} mdls args
    "-XCPP"     -> decodeArgs f{doCPP = True} mdls args
    "-z"        -> decodeArgs f{compress = True} mdls args
    "-b64"      -> decodeArgs f{base64 = True} mdls args
    "-Q"        -> decodeArgs f{installPkg = True} mdls args
    "-o" | s : args' <- args
                -> decodeArgs f{output = s} mdls args'
    "-optc" | s : args' <- args
                -> decodeArgs f{cArgs = cArgs f ++ [s]} mdls args'
    "-optl" | s : args' <- args
                -> decodeArgs f{lArgs = lArgs f ++ [s]} mdls args'
    '-':'i':[]  -> decodeArgs f{paths = []} mdls args
    '-':'i':s   -> decodeArgs f{paths = paths f ++ [s]} mdls args
    '-':'o':s   -> decodeArgs f{output = s} mdls args
    '-':'t':s   -> decodeArgs f{target = s} mdls args
    '-':'D':_   -> decodeArgs f{cppArgs = cppArgs f ++ [arg]} mdls args
    '-':'I':_   -> decodeArgs f{cppArgs = cppArgs f ++ [arg]} mdls args
    '-':'P':s   -> decodeArgs f{buildPkg = Just s} mdls args
    '-':'a':[]  -> decodeArgs f{pkgPath = []} mdls args
    '-':'a':s   -> decodeArgs f{pkgPath = pkgPath f ++ [s]} mdls args
    '-':'L':s   -> decodeArgs f{listPkg = Just s} mdls args
    '-':'p':s   -> decodeArgs f{preload = preload f ++ [s]} mdls args
    '-':'d':'d':'u':'m':'p':'-':r | Just d <- lookup r dumpFlagTable ->
                   decodeArgs f{dumpFlags = d : dumpFlags f} mdls args
    "--stdin"   -> decodeArgs f{useStdin = True} mdls args
    '-':_       -> mhsError $ "Unknown flag: " ++ arg ++ "\n" ++ usage
    _ | arg `hasTheExtension` ".c" || arg `hasTheExtension` ".o" || arg `hasTheExtension` ".a"
                -> decodeArgs f{cArgs = cArgs f ++ [arg]} mdls args
      | otherwise
                -> decodeArgs f (mdls ++ [arg]) args
  where
    dumpFlagTable = [(drop 1 $ show d, d) | d <- [minBound..maxBound]]

readTargets :: Flags -> FilePath -> IO [Target]
readTargets flags dir = do
  let tgFilePath = dir </> "targets.conf"
  exists <- doesFileExist tgFilePath
  if not exists
     then return []
     else do
       tgFile <- readFile tgFilePath
       case parseTargets tgFilePath tgFile of
         Left e -> do
           putStrLn $ "Cannot parse " ++ tgFilePath
           when (verbosityGT flags 0) $
             putStrLn e
           return []
         Right tgs -> do
           when (verbosityGT flags 0) $
             putStrLn $ "Read targets file. Possible targets: " ++ show
               [tg | Target tg _ <- tgs]
           return tgs

readTarget :: Flags -> FilePath -> IO TTarget
readTarget flags dir = do
  targets <- readTargets flags dir
  (n, cs) <-
    case findTarget (target flags) targets of
      Nothing -> do
        when (verbosityGT flags 0) $
          putStrLn $ unwords ["Warning: could not find", target flags, "in file"]
        return ("default", [])
      Just (Target n cs) -> do
        when (verbosityGT flags 0) $
          putStrLn $ "Found target: " ++ show cs
        return (n, cs)
  return TTarget { tName    = n
                 , tCC      = fromMaybe "cc"   $ lookup "cc"      cs
                 , tCCFlags = fromMaybe ""     $ lookup "ccflags" cs
                 , tCCLibs  = fromMaybe ""     $ lookup "cclibs"  cs
                 , tConf    = fromMaybe "unix" $ lookup "conf"    cs
                 , tOut     = fromMaybe "-o"   $ lookup "cout"    cs
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
      pkg = Package { pkgName = mkIdent name
                    , pkgVersion = ver
                    , pkgCompiler = mhsVersion
                    , pkgOptl = lArgs flags
                    , pkgExported = exported
                    , pkgOther = other
                    , pkgTables = getCacheTables cash
                    , pkgDepends = pkgDeps }
  --print (map tModuleName $ pkgOther pkg)
  t1 <- getTimeMilli
  when (verbose flags > 0) $
    putStrLn $ "Writing package " ++ namever ++ " to " ++ output flags
  writeSerializedCompressed (output flags) (forcePackage pkg)
  t2 <- getTimeMilli
  when (verbose flags > 0) $
    putStrLn $ "Compression time " ++ show (t2 - t1) ++ " ms"

splitNameVer :: String -> (String, Version)
splitNameVer s =
  case span (\ c -> isDigit c || c == '.') (reverse s) of
    (rver, '-':rname) | not (null is)  -> (reverse rname, makeVersion is)
      where is = readVersion (reverse rver)
    _ -> mhsError $ "package name not of the form name-version:" ++ show s
  where readVersion = map readInt . words . map (\ c -> if c == '.' then ' ' else c)

-- Take a file name of a package, or just a package name,
-- return the full name of the package file.
-- It's an error if no package can be found.
findAPackage :: Flags -> FilePath -> IO FilePath
findAPackage flags pkgnm = do
  ok <- doesFileExist pkgnm
  if ok then
    return pkgnm
   else do
    dirpkgs <- findAllPackages flags
    case [ pdir </> pkg <.> packageSuffix | (pdir, pkgs) <- dirpkgs, pkg <- pkgs, pkgnm `isPrefixOf` pkg ] of
      [] -> mhsError $ "Package not found: " ++ show pkgnm
      [s] -> return s
      ss -> mhsError $ "Package not is ambigous: " ++ show (pkgnm, ss)

mainListPkg :: Flags -> FilePath -> IO ()
mainListPkg flags "" = mainListPackages flags
mainListPkg flags pkgnm = do
  pkgfn <- findAPackage flags pkgnm
  pkg <- readSerialized pkgfn
  putStrLn $ "name: " ++ showIdent (pkgName pkg)
  putStrLn $ "version: " ++ showVersion (pkgVersion pkg)
  putStrLn $ "compiler: mhs-" ++ pkgCompiler pkg
  putStrLn $ "depends: " ++ unwords (map (\ (i, v) -> showIdent i ++ "-" ++ showVersion v) (pkgDepends pkg))
  putStrLn $ "linker opts: " ++ unwords (pkgOptl pkg)

  let oneMdl tmdl = do
        putStrLn $ "  " ++ showIdent (tModuleName tmdl)
        when (verbosityGT flags 0) $
          printExperted tmdl
        when (verbosityGT flags 1) $
          printDefinitions tmdl
  putStrLn "exposed-modules:"
  mapM_ oneMdl (pkgExported pkg)
  putStrLn "other-modules:"
  mapM_ oneMdl (pkgOther pkg)

printExperted :: TModule a -> IO ()
printExperted tmdl = do
  putStrLn "  values:"
  mapM_ (putStrLn . ("    " ++) . showValueExport) (tValueExps tmdl)
  putStrLn "  types:"
  mapM_ printTypeExport (tTypeExps tmdl)

printTypeExport :: TypeExport -> IO ()
printTypeExport te = do
  putStrLn $ "    " ++ showTypeExport te
  putStrLn $ unlines $ map ("      " ++) (showTypeExportAssocs te)

printDefinitions :: TModule [(Ident, a)]-> IO ()
printDefinitions tmdl = do
  putStrLn "  top level definitions"
  putStrLn $ unwords (map (showIdent . fst) $ tBindingsOf tmdl)

mainCompile :: Flags -> Ident -> IO ()
mainCompile flags mn = do
  t0 <- getTimeMilli
  (cash, (rmn, allDefs)) <- do
    cash <- getCached flags
    (rds, _, cash') <- compileCacheTop flags mn cash
    maybeSaveCache flags cash'
    return (cash', rds)

  t1 <- getTimeMilli
  let
    mainName = qualIdent rmn (mkIdent "main")
    cmdl = (allDefs, if noLink flags then Lit (LInt 0) else Var mainName)
    (forExps, outCMdl@(outDefs, _)) = renumberCMdl cmdl
    outData = toStringCMdl outCMdl
    numOutDefs = length outData
    numDefs = length allDefs
  when (verbosityGT flags 0) $
    putStrLn $ "top level defns:      " ++ padLeft 6 (show numOutDefs) ++ " (unpruned " ++ show numDefs ++ ")"
  let printLDefs = mapM_ (\ (i, e) -> putStrLn $ showIdent i ++ " = " ++ toStringP e "")
  dumpIf flags Dlinked $ do
    putStrLn "linked:"; printLDefs (removeUnused cmdl)
  dumpIf flags Dtoplevel $ do
    putStrLn "toplevel:"; printLDefs allDefs
  if runIt flags then do
    if compiledWithMhs then do
      let prg = translateAndRun cmdl
      prg
     else if compiledWithGhc then
      withMhsContext $ \ ctx -> do
        run ctx outData
      else mhsError "The -r flag currently only works with mhs and ghc"
   else do
    seq (length outData) (return ())
    t2 <- getTimeMilli
    when (verbosityGT flags 0) $
      putStrLn $ "final pass            " ++ padLeft 6 (show (t2-t1)) ++ "ms"

    when (speed flags) $ do
      let fns = filter (isSuffixOf ".hs") $ map (slocFile . slocIdent) $ cachedNonPkgModuleNames cash
      locs <- sum . map (length . lines) <$> mapM readFile fns
      putStrLn $ show (locs * 1000 `div` (t2 - t0)) ++ " lines/s"

    let (cFFI, hFFI) = makeFFI flags forExps outDefs
        cCode = "#include \"mhsffi.h\"\n" ++ makeCArray flags outData ++ cFFI

    let outFile = output flags
    -- Generate stub file for 'foreign export'
    unless (null forExps) $ do
      let stubName = takeDirectory outFile </> dropExtension (showIdent mn) ++ "_stub.h"
      when (verbosityGT flags 0) $
        putStrLn $ "generate stub: " ++ stubName
      writeFile stubName hFFI
    -- Decode what to do:
    --  * file ends in .comb: write combinator file
    --  * file ends in .c: write C version of combinator
    --  * otherwise, write C file and compile to a binary with cc
    if outFile `hasTheExtension` ".comb" then do
      h <- openBinaryFile outFile WriteMode
      h' <- if base64 flags then do addBase64 h else return h
      h'' <- if compress flags then do hPutChar h' 'z'; addLZ77 h' else return h'
      hPutStr h'' outData
      hClose h''
     else if outFile `hasTheExtension` ".c" then
      writeFile outFile cCode
     else do
       (fn, h) <- openTmpFile "mhsc.c"
       let ppkgs = getPathPkgs cash
       hPutStr h cCode
       hClose h
       mainCompileC flags ppkgs fn
       removeFile fn

mainCompileC :: Flags -> [(FilePath, Package)] -> FilePath -> IO ()
mainCompileC flags pkgs infile = do
  let ppkgs  = map fst pkgs
      poptls = filter (not . null . pkgOptl) $ map snd pkgs
  ct1 <- getTimeMilli
  let dir = mhsdir flags
      incDirs = map (convertToInclude "include") ppkgs
      cDirs   = map (convertToInclude "cbits") ppkgs
      outFile = output flags
  incDirs' <- filterM doesDirectoryExist incDirs
  cDirs'   <- filterM doesDirectoryExist cDirs
  -- print (map fst $ getPathPkgs cash, (incDirs, incDirs'), (cDirs, cDirs'))
  let incs = unwords $ map ("-I" ++) incDirs'
      defs = "-D__MHS__"
      cpps = concatMap (\ a -> "'" ++ a ++ "' ") (cppArgs flags)  -- Use all CPP args from the command line
      rtdir = dir ++ "/src/runtime"
  tgt <- readTarget flags dir
  let optls = concatMap pkgOptl poptls -- optl from pkgs
      cmd = unwords $ [tCC tgt,
                       tCCFlags tgt,
                       "-I" ++ rtdir,
                       "-I" ++ rtdir </> tConf tgt,
                       incs,
                       defs,
                       cpps] ++
                       cArgs flags ++
                       lArgs flags ++
                       optls ++
                       map (++ "/*.c") cDirs' ++
                      [ rtdir </> "main.c" | not (noLink flags) ] ++
                      [ rtdir </> "eval.c",
                        infile,
                        tCCLibs tgt,
                        tOut tgt ++ outFile
                      ]
  when (verbosityGT flags 0) $
    putStrLn $ "Execute: " ++ show cmd
  ec <- system cmd
  when (ec /= ExitSuccess) $
    mhsError $ "command failed: " ++ cmd
  ct2 <- getTimeMilli
  when (verbosityGT flags 0) $
    putStrLn $ "C compilation         " ++ padLeft 6 (show (ct2-ct1)) ++ "ms"

mainInstallPackage :: Flags -> [FilePath] -> IO ()
mainInstallPackage flags [pkgfn, dir] = do
  when (verbosityGT flags (-1)) $
    putStrLn $ "Installing package " ++ pkgfn ++ " in " ++ dir
  pkg <- readSerialized pkgfn
  let pdir = dir </> packageDir
      pkgout = unIdent (pkgName pkg) ++ "-" ++ showVersion (pkgVersion pkg) <.> packageSuffix
  createDirectoryIfMissing True pdir
  copyFile pkgfn (pdir </> pkgout)
  let mk tm = do
        let fn = dir </> moduleToFile (tModuleName tm) <.> packageTxtSuffix
            dn = takeDirectory fn
        when (verbosityGT flags 2) $
          putStrLn $ "create " ++ fn
        createDirectoryIfMissing True dn
        writeFile fn pkgout
  mapM_ mk (pkgExported pkg)
mainInstallPackage flags [pkgfn] =
  case pkgPath flags of
    [] -> mhsError "pkgPath is empty"
    frst:_ -> mainInstallPackage flags [pkgfn, frst]
mainInstallPackage _ _ = mhsError usage

findAllPackages :: Flags -> IO [(FilePath, [String])]
findAllPackages flags = concat <$> mapM list (pkgPath flags)
  where list dir = do
          let pdir = dir </> packageDir
          ok <- doesDirectoryExist pdir
          if ok then do
            files <- getDirectoryContents pdir
            let pkgs = [ b | f <- files, Just b <- [stripSuffix packageSuffix f] ]
            if null pkgs then
              return []
             else
              return [(pdir, pkgs)]
           else
            return []

mainListPackages :: Flags -> IO ()
mainListPackages flags = mapM_ one =<< findAllPackages flags
  where one (pdir, pkgs) = do
          putStrLn $ pdir ++ ":"
          mapM_ (\ p -> putStrLn $ "  " ++ p) pkgs

-- Convert something like
--   .../.mcabal/mhs-0.10.3.0/packages/base-0.10.3.0.pkg
-- into
--   .../.mcabal/mhs-0.10.3.0/packages/base-0.10.3.0/include
convertToInclude :: String -> FilePath -> FilePath
convertToInclude inc pkg = dropExtension pkg </> inc

hasTheExtension :: FilePath -> String -> Bool
hasTheExtension f e = e `isSuffixOf` f
