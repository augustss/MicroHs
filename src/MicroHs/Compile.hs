-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Compile(
  compileCacheTop,
  mhsCacheName,
  Flags(..), verbose, runIt, output, useCache,
  getCached,
  validateCache,
  Cache, emptyCache, deleteFromCache,
  ) where
import Prelude
import Data.Maybe
import System.IO
import System.IO.MD5
import Control.DeepSeq
import MicroHs.Abstract
--import MicroHs.AbstractOleg()
import MicroHs.CompileCache
import MicroHs.Desugar
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.TypeCheck
import Compat
import MicroHs.Instances() -- for ghc

mhsCacheName :: FilePath
mhsCacheName = ".mhscache"

data Flags = Flags
  Int        -- verbosity level
  Bool       -- run instead of compile
  [String]   -- module search path
  String     -- output file
  Bool       -- show loading message
  Bool       -- use caching
  deriving (Show)

type Time = Int

verbose :: Flags -> Int
verbose (Flags x _ _ _ _ _) = x

runIt :: Flags -> Bool
runIt (Flags _ x _ _ _ _) = x

paths :: Flags -> [String]
paths (Flags _ _ x _ _ _) = x

output :: Flags -> String
output (Flags _ _ _ x _ _) = x

loading :: Flags -> Bool
loading (Flags _ _ _ _ x _) = x

useCache :: Flags -> Bool
useCache (Flags _ _ _ _ _ x) = x

-----------------

-- Compile the module with the given name, starting with the given cache.
-- Return the "compiled module" and the resulting cache.
compileCacheTop :: Flags -> IdentModule -> Cache -> IO ([(Ident, Exp)], Cache)
compileCacheTop flags mn ch = do
  (ds, ch') <- compile flags mn ch
  t1 <- getTimeMilli
  let
    dsn = [ (n, compileOpt e) | (n, e) <- ds ]
  () <- return (rnf dsn)
  t2 <- getTimeMilli
  when (verbose flags > 0) $
    putStrLn $ "combinator conversion " ++ padLeft 6 (show (t2-t1)) ++ "ms"
  when (verbose flags > 3) $
    putStrLn $ "combinators:\n" ++ showLDefs dsn
  return (dsn, ch')

getCached :: Flags -> IO Cache
getCached flags | not (useCache flags) = return emptyCache
getCached flags = do
  mhin <- openFileM mhsCacheName ReadMode
  case mhin of
    Nothing -> return emptyCache
    Just hin -> do
      when (loading flags || verbose flags > 0) $
        putStrLn $ "Loading saved cache " ++ show mhsCacheName
--      putStrLn "deserialize cache"
      cash <- hDeserialize hin
      hClose hin
      validateCache flags cash

compile :: Flags -> IdentModule -> Cache -> IO ([LDef], Cache)
compile flags nm ach = do
  ((_, t), ch) <- runStateIO (compileModuleCached flags nm) ach
  when (verbose flags > 0) $
    putStrLn $ "total import time     " ++ padLeft 6 (show t) ++ "ms"
  return (concatMap bindingsOf $ map tModuleOf $ M.elems $ cache ch, ch)

-- Compile a module with the given name.
-- If the module has already been compiled, return the cached result.
compileModuleCached :: Flags -> IdentModule -> StateIO Cache (CModule, Time)
compileModuleCached flags mn = do
  ch <- gets cache
  case M.lookup mn ch of
    Nothing -> do
      ws <- gets working
      when (elem mn ws) $
        error $ "recursive module: " ++ showIdent mn ++ ", import chain: " ++ unwords (map showIdent ws)
      modify $ \ c -> updWorking (mn : working c) c
      when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing " ++ showIdent mn
      (cm, tp, tt, ts) <- compileModule flags mn
      when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing done " ++ showIdent mn ++ ", " ++ show (tp + tt) ++
                 "ms (" ++ show tp ++ " + " ++ show tt ++ ")"
      when (loading flags && mn /= mkIdent "Interactive") $
        liftIO $ putStrLn $ "loaded " ++ showIdent mn
      cash <- get
      put $ workToDone cm cash
      return (cm, tp + tt + ts)
    Just cm -> do
      when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing cached " ++ showIdent mn
      return (cm, 0)

-- Find and compile a module with the given name.
-- The times are (parsing, typecheck+desugar, imported modules)
compileModule :: Flags -> IdentModule -> StateIO Cache (CModule, Time, Time, Time)
compileModule flags nm = do
  t1 <- liftIO getTimeMilli
  (pathfn, file) <- liftIO (readModulePath flags nm)
  mchksum <- liftIO (md5File pathfn)  -- XXX there is a small gap between reading and computing the checksum.
  let chksum :: MD5CheckSum
      chksum = fromMaybe undefined mchksum
  let mdl@(EModule nmn _ defs) = parseDie pTop pathfn file
  -- liftIO $ putStrLn $ showEModule mdl
  -- liftIO $ putStrLn $ showEDefs defs
  when (nm /= nmn) $
    error $ "module name does not agree with file name: " ++ showIdent nm ++ " " ++ showIdent nmn
  let
    specs = [ s | Import s <- defs ]
    imported = [ m | ImportSpec _ m _ _ <- specs ]
  t2 <- liftIO getTimeMilli
  (impMdls, ts) <- fmap unzip $ mapM (compileModuleCached flags) imported
  t3 <- liftIO getTimeMilli
  let
    tmdl = typeCheck (zip specs (map tModuleOf impMdls)) mdl
  when (verbose flags > 2) $
    liftIO $ putStrLn $ "type checked:\n" ++ showTModule showEDefs tmdl ++ "-----\n"
  let
    dmdl = desugar tmdl
  () <- return $ rnf $ bindingsOf dmdl
  t4 <- liftIO getTimeMilli
  when (verbose flags > 3) $
    (liftIO $ putStrLn $ "desugared:\n" ++ showTModule showLDefs dmdl)
  let cmdl = CModule dmdl imported chksum
  return (cmdl, t2-t1, t4-t3, sum ts)

validateCache :: Flags -> Cache -> IO Cache
validateCache flags cash = execStateIO (mapM_ validate (M.keys (cache cash))) cash
  where
    deps = invertGraph [ (tModuleName tm, imps) | CModule tm imps _ <- M.elems (cache cash) ]
    invalidate :: IdentModule -> StateIO Cache ()
    invalidate mn = do
      b <- gets $ isJust . M.lookup mn . cache
      when b $ do
        -- It's still in the cache, so invalidate it, and all modules that import it
        when (verbose flags > 0) $
          liftIO $ putStrLn $ "invalidate cached " ++ show mn
        modify (deleteFromCache mn)
        mapM_ invalidate $ fromMaybe [] $ M.lookup mn deps
    validate :: IdentModule -> StateIO Cache ()
    validate mn = do
      ch <- get
      case M.lookup mn (cache ch) of
        Nothing -> return () -- no longer in the cache, so just ignore.
        Just (CModule _ _ chksum) -> do
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

readModulePath :: Flags -> IdentModule -> IO (FilePath, String)
readModulePath flags mn = do
  mh <- findModulePath flags mn
  case mh of
    Nothing -> errorMessage (getSLoc mn) $ "File not found: " ++ show mn ++ "\npath=" ++ show (paths flags)
    Just (fn, h) -> do
      file <- hGetContents h
      return (fn, file)

findModulePath :: Flags -> IdentModule -> IO (Maybe (FilePath, Handle))
findModulePath flags mn = do
  let
    fn = map (\ c -> if c == '.' then '/' else c) (unIdent mn) ++ ".hs"
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
