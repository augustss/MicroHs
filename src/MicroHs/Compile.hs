-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Compile(
  compileTop,
  Flags(..), verbose, runIt, output,
  compileCacheTop,
  Cache, emptyCache, deleteFromCache,
  ) where
import Prelude
import System.IO
import Control.DeepSeq
import qualified MicroHs.IdentMap as M
import MicroHs.StateIO
import MicroHs.Desugar
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Ident
import MicroHs.Parse
import MicroHs.TypeCheck
--Ximport Compat

data Flags = Flags Int Bool [String] String Bool
  --Xderiving (Show)

type Time = Int

verbose :: Flags -> Int
verbose (Flags x _ _ _ _) = x

runIt :: Flags -> Bool
runIt (Flags _ x _ _ _) = x

paths :: Flags -> [String]
paths (Flags _ _ x _ _) = x

output :: Flags -> String
output (Flags _ _ _ x _) = x

loading :: Flags -> Bool
loading (Flags _ _ _ _ x) = x

-----------------

type CModule = TModule [LDef]
data Cache = Cache [IdentModule] (M.Map CModule)
  --Xderiving (Show)

working :: Cache -> [IdentModule]
working (Cache x _) = x

updWorking :: [IdentModule] -> Cache -> Cache
updWorking w (Cache _ m) = Cache w m

cache :: Cache -> M.Map CModule
cache (Cache _ x) = x

emptyCache :: Cache
emptyCache = Cache [] M.empty

deleteFromCache :: Ident -> Cache -> Cache
deleteFromCache mn (Cache is m) = Cache is (M.delete mn m)

-----------------

compileCacheTop :: Flags -> Ident -> Cache -> IO ([(Ident, Exp)], Cache)
compileCacheTop flags mn ch = do
  (ds, ch') <- compile flags mn ch
  t1 <- getTimeMilli
  let
    dsn = [ (n, compileOpt e) | (n, e) <- ds ]
  () <- return (rnf dsn)
  t2 <- getTimeMilli
  when (verbose flags > 0) $
    putStrLn $ "combinator conversion " ++ padLeft 6 (show (t2-t1)) ++ "ms"
  return (dsn, ch')

--compileTop :: Flags -> IdentModule -> IO [LDef]
compileTop :: Flags -> Ident -> IO [(Ident, Exp)]
compileTop flags mn = fmap fst $ compileCacheTop flags mn emptyCache

compile :: Flags -> IdentModule -> Cache -> IO ([LDef], Cache)
compile flags nm ach = do
  ((_, t), ch) <- runStateIO (compileModuleCached flags nm) ach
  when (verbose flags > 0) $
    putStrLn $ "total import time     " ++ padLeft 6 (show t) ++ "ms"
  return (concatMap bindingsOf $ M.elems $ cache ch, ch)

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
      c <- get
      put $ Cache (tail (working c)) (M.insert mn cm (cache c))
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
  let
    fn = map (\ c -> if c == '.' then '/' else c) (unIdent nm) ++ ".hs"
  (pathfn, file) <- liftIO (readFilePath (getSLoc nm) (paths flags) fn)
  let mdl@(EModule nmn _ defs) = parseDie pTop pathfn file
  -- liftIO $ putStrLn $ showEModule mdl
  -- liftIO $ putStrLn $ showEDefs defs
  when (nm /= nmn) $
    error $ "module name does not agree with file name: " ++ showIdent nm ++ " " ++ showIdent nmn
  let
    specs = [ s | Import s <- defs ]
  t2 <- liftIO getTimeMilli
  (impMdls, ts) <- fmap unzip $ mapM (compileModuleCached flags) [ m | ImportSpec _ m _ _ <- specs ]
  t3 <- liftIO getTimeMilli
  let
    tmdl = typeCheck (zip specs impMdls) mdl
  when (verbose flags > 2) $
    liftIO $ putStrLn $ "type checked:\n" ++ showTModule showEDefs tmdl ++ "-----\n"
  let
    dmdl = desugar tmdl
  () <- return $ rnf $ bindingsOf dmdl
  t4 <- liftIO getTimeMilli
  when (verbose flags > 2) $
    (liftIO $ putStrLn $ "desugared:\n" ++ showTModule showLDefs dmdl)
  return (dmdl, t2-t1, t4-t3, sum ts)

------------------

readFilePath :: SLoc -> [FilePath] -> FilePath -> IO (FilePath, String)
readFilePath loc path name = do
  mh <- openFilePath path name
  case mh of
    Nothing -> errorMessage loc $ "File not found: " ++ show name ++ "\npath=" ++ show path
    Just (fn, h) -> do
      file <- hGetContents h
      return (fn, file)

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
