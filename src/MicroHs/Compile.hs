-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Compile(
  compileCacheTop,
  compileMany,
  mhsCacheName,
  getCached,
  validateCache,
  Cache, emptyCache, deleteFromCache,
  ) where
import Prelude
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import System.IO.MD5
import System.IO.Serialize
import System.Process
import Control.DeepSeq
import MicroHs.Abstract
import MicroHs.CompileCache
import MicroHs.Desugar
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Flags
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.TypeCheck
import Compat
import MicroHs.Instances() -- for ghc

mhsCacheName :: FilePath
mhsCacheName = ".mhscache"

type Time = Int

-----------------

-- Compile the module with the given name, starting with the given cache.
-- Return the "compiled module" and the resulting cache.
compileCacheTop :: Flags -> IdentModule -> Cache -> IO ((IdentModule, [(Ident, Exp)]), Cache)
compileCacheTop flags mn ch = do
  ((rmn, ds), ch') <- compile flags mn ch
  -- get loaded packages
  -- recursively load all dependencies
  -- add everything to ds
  t1 <- getTimeMilli
  let
    dsn = [ (n, compileOpt e) | (n, e) <- ds ]
  () <- return (rnf dsn)
  t2 <- getTimeMilli
  when (verbosityGT flags 0) $
    putStrLn $ "combinator conversion " ++ padLeft 6 (show (t2-t1)) ++ "ms"
  when (verbosityGT flags 3) $
    putStrLn $ "combinators:\n" ++ showLDefs dsn
  return ((rmn, dsn), ch')

compileMany :: Flags -> [IdentModule] -> Cache -> IO Cache
compileMany flags mns ach = snd <$> runStateIO (mapM_ (compileModuleCached flags) mns) ach

getCached :: Flags -> IO Cache
getCached flags | not (readCache flags) = return emptyCache
getCached flags = do
  mcash <- loadCached mhsCacheName
  case mcash of
    Nothing ->
      return emptyCache
    Just cash -> do
      when (loading flags || verbosityGT flags 0) $
        putStrLn $ "Loading saved cache " ++ show mhsCacheName
      validateCache flags cash

compile :: Flags -> IdentModule -> Cache -> IO ((IdentModule, [LDef]), Cache)
compile flags nm ach = do
  ((cm, t), ch) <- runStateIO (compileModuleCached flags nm) ach
  when (verbosityGT flags 0) $
    putStrLn $ "total import time     " ++ padLeft 6 (show t) ++ "ms"
  return ((tModuleName cm, concatMap bindingsOf $ cachedModules ch), ch)

-- Compile a module with the given name.
-- If the module has already been compiled, return the cached result.
-- If the module has not been compiled, first try to find a source file.
-- If there is no source file, try loading a package.
compileModuleCached :: Flags -> IdentModule -> StateIO Cache (TModule [LDef], Time)
compileModuleCached flags mn = do
  cash <- get
  case lookupCache mn cash of
    Nothing -> do
      modify $ addWorking mn
      when (verbosityGT flags 0) $
        liftIO $ putStrLn $ "importing " ++ showIdent mn
      mres <- liftIO (readModulePath flags mn)
      case mres of
        Nothing -> findPkgModule flags mn
        Just (pathfn, file) -> compileModule flags mn pathfn file
    Just tm -> do
      when (verbosityGT flags 0) $
        liftIO $ putStrLn $ "importing cached " ++ showIdent mn
      return (tm, 0)

compileModule :: Flags -> IdentModule -> FilePath -> String -> StateIO Cache (TModule [LDef], Time)
compileModule flags mn pathfn file = do
  t1 <- liftIO getTimeMilli
  mchksum <- liftIO (md5File pathfn)  -- XXX there is a small gap between reading and computing the checksum.
  let chksum :: MD5CheckSum
      chksum = fromMaybe undefined mchksum
  let pmdl = parseDie pTop pathfn file
      mdl@(EModule mnn _ defs) = addPreludeImport pmdl
  
  -- liftIO $ putStrLn $ showEModule mdl
  -- liftIO $ putStrLn $ showEDefs defs
  -- TODO: skip test when mn is a file name
  when (isNothing (getFileName mn) && mn /= mnn) $
    error $ "module name does not agree with file name: " ++ showIdent mn ++ " " ++ showIdent mnn
  let
    specs = [ s | Import s <- defs ]
    imported = [ m | ImportSpec _ m _ _ <- specs ]
  t2 <- liftIO getTimeMilli
  (impMdls, its) <- fmap unzip $ mapM (compileModuleCached flags) imported
  t3 <- liftIO getTimeMilli
  let
    tmdl = typeCheck (zip specs impMdls) mdl
  when (verbosityGT flags 2) $
    liftIO $ putStrLn $ "type checked:\n" ++ showTModule showEDefs tmdl ++ "-----\n"
  let
    dmdl = desugar flags tmdl
  () <- return $ rnf $ bindingsOf dmdl
  t4 <- liftIO getTimeMilli
  let tp = t2 - t1
      tt = t4 - t3
      ts = sum its
  when (verbosityGT flags 3) $
    (liftIO $ putStrLn $ "desugared:\n" ++ showTModule showLDefs dmdl)
  when (verbosityGT flags 0) $
    liftIO $ putStrLn $ "importing done " ++ showIdent mn ++ ", " ++ show (tp + tt) ++
            "ms (" ++ show tp ++ " + " ++ show tt ++ ")"
  when (loading flags && mn /= mkIdent "Interactive") $
    liftIO $ putStrLn $ "loaded " ++ showIdent mn
  modify $ workToDone (dmdl, imported, chksum)
  return (dmdl, tp + tt + ts)

addPreludeImport :: EModule -> EModule
addPreludeImport (EModule mn es ds) =
  EModule mn es ds'
  where ds' = ps' ++ nps
        (ps, nps) = partition isImportPrelude ds
        isImportPrelude (Import (ImportSpec _ i _ _)) = i == idPrelude
        isImportPrelude _ = False
        idPrelude = mkIdent "Prelude"
        ps' =
          case ps of
            [] -> [Import $ ImportSpec False idPrelude Nothing Nothing]     -- no Prelude imports, so add 'import Prelude'
            [Import (ImportSpec False _ Nothing (Just (False, [])))] -> []  -- exactly 'import Prelude()', so import nothing
            _ -> ps                                                         -- keep the given Prelude imports

-------------------------------------------

validateCache :: Flags -> Cache -> IO Cache
validateCache flags acash = execStateIO (mapM_ (validate . fst) fdeps) acash
  where
    fdeps = getImportDeps acash                           -- forwards dependencies
    deps = invertGraph fdeps                              -- backwards dependencies
    invalidate :: IdentModule -> StateIO Cache ()
    invalidate mn = do
      b <- gets $ isJust . lookupCache mn
      when b $ do
        -- It's still in the cache, so invalidate it, and all modules that import it
        when (verbosityGT flags 0) $
          liftIO $ putStrLn $ "invalidate cached " ++ show mn
        modify (deleteFromCache mn)
        mapM_ invalidate $ fromMaybe [] $ M.lookup mn deps
    validate :: IdentModule -> StateIO Cache ()
    validate mn = do
      cash <- get
      case lookupCacheChksum mn cash of
        Nothing -> return () -- no longer in the cache, so just ignore.
        Just chksum -> do
          mhdl <- liftIO $ findModulePath flags mn
          case mhdl of
            Nothing ->
              -- Cannot find module, so invalidate it
              invalidate mn
            Just (_, h) -> do
              cs <- liftIO $ md5Handle h
              liftIO $ hClose h
              when (cs /= chksum) $
                -- bad checksum, invalidate module
                invalidate mn

-- Take a graph in adjencency list form and reverse all the arrow.
-- Used to invert the import graph.
invertGraph :: [(IdentModule, [IdentModule])] -> M.Map [IdentModule]
invertGraph = foldr ins M.empty
  where
    ins :: (IdentModule, [IdentModule]) -> M.Map [IdentModule] -> M.Map [IdentModule]
    ins (m, ms) g = foldr (\ n -> M.insertWith (++) n [m]) g ms

------------------

-- Is the module name actually a file name?
getFileName :: IdentModule -> Maybe String
getFileName m | ".hs" `isSuffixOf` s = Just s
             | otherwise = Nothing
  where s = unIdent m

readModulePath :: Flags -> IdentModule -> IO (Maybe (FilePath, String))
readModulePath flags mn | Just fn <- getFileName mn = do
  mh <- openFileM fn ReadMode
  case mh of
    Nothing -> errorMessage (getSLoc mn) $ "File not found: " ++ show fn
    Just h -> readRest fn h
                        | otherwise = do
  mh <- findModulePath flags mn
  case mh of
    Nothing -> return Nothing
    Just (fn, h) -> readRest fn h
  where readRest fn h = do
          file <-
            if doCPP flags then do
              hClose h
              runCPPTmp flags fn
            else
              hGetContents h
          return (Just (fn, file))


moduleToFile :: IdentModule -> FilePath
moduleToFile mn = map (\ c -> if c == '.' then '/' else c) (unIdent mn)

findModulePath :: Flags -> IdentModule -> IO (Maybe (FilePath, Handle))
findModulePath flags mn = do
  let
    fn = moduleToFile mn ++ ".hs"
  openFilePath (paths flags) fn

openFilePath :: [FilePath] -> FilePath -> IO (Maybe (FilePath, Handle))
openFilePath adirs fileName =
  case adirs of
    [] -> return Nothing
    dir:dirs -> do
      let
        path = dir ++ "/" ++ fileName
      mh <- openFileM path ReadMode
      case mh of
        Nothing -> openFilePath dirs fileName -- If opening failed, try the next directory
        Just hdl -> return (Just (path, hdl))

runCPPTmp :: Flags -> FilePath -> IO String
runCPPTmp flags infile = do
  (fn, h) <- openTmpFile "mhscpp.hs"
  runCPP flags infile fn
  file <- hGetContents h
  removeFile fn
  return file

mhsDefines :: [String]
mhsDefines =
  [ "'-DMIN_VERSION_base(x,y,z)=(x<=4||y<=14)'" -- Pretend we have base version 4.14
  , "-D__MHS__"                                 -- We are MHS
  ]

runCPP :: Flags -> FilePath -> FilePath -> IO ()
runCPP flags infile outfile = do
  mcpphs <- lookupEnv "MHSCPPHS"
  let cpphs = fromMaybe "cpphs" mcpphs
      args = mhsDefines ++ cppArgs flags
      cmd = cpphs ++ " --noline " ++ unwords args ++ " " ++ infile ++ " -O" ++ outfile
  when (verbosityGT flags 0) $
    putStrLn $ "Execute: " ++ show cmd
  callCommand cmd

findPkgModule :: Flags -> IdentModule -> StateIO Cache (TModule [LDef], Time)
findPkgModule flags mn = do
  let fn = moduleToFile mn
  mres <- liftIO $ openFilePath (pkgPath flags) fn
  case mres of
    Just (pfn, hdl) -> do
      pkg <- liftIO $ hGetContents hdl
      liftIO $ hClose hdl
      let dir = take (length pfn - length fn) pfn  -- directory where the file was found
      loadPkg flags dir pkg
      cash <- get
      case lookupCache mn cash of
        Nothing -> error $ "package does not contain module " ++ pkg ++ " " ++ showIdent mn
        Just t -> return (t, 0)
    Nothing ->
      errorMessage (getSLoc mn) $
        "Module not found: " ++ show mn ++
        "\nsearch path=" ++ show (paths flags) ++
        "\npackage path=" ++ show (pkgPath flags)

loadPkg :: Flags -> FilePath -> FilePath -> StateIO Cache ()
loadPkg _flags dir fn = do
  pkg <- liftIO $ readSerialized (dir ++ "/packages/" ++ fn)
  modify $ addPackage pkg

-- XXX add function to find&load package from package name
