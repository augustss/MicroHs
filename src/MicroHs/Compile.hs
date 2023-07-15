-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Compile where
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map as M
import System.IO

import MicroHs.Desugar
import MicroHs.Parse

data Flags = Flags {
  verbose :: Bool,
  runIt   :: Bool,
  paths   :: [FilePath]
  }
  deriving (Show)

data Cache = Cache
  { working :: [IdentModule]
  , cache   :: M.Map IdentModule Module
  }
  deriving (Show)

compile :: Flags -> IdentModule -> IO [LDef]
compile flags nm = do
  ch <- execStateT (compileModuleCached flags nm) Cache{ working = [], cache = M.empty }
  let defs (Module _ _ _ ds) = ds
  pure $ concatMap defs $ M.elems $ cache ch

compileModuleCached :: Flags -> IdentModule -> StateT Cache IO Module
compileModuleCached flags nm = do
  ch <- gets cache
  case M.lookup nm ch of
    Nothing -> do
      ws <- gets working
      when (nm `elem` ws) $
        error $ "recursive module: " ++ show nm
      modify $ \ c -> c { working = nm : working c }
      when (verbose flags) $
        liftIO $ putStrLn $ "importing " ++ show nm
      cm <- compileModule flags nm
      when (verbose flags) $
        liftIO $ putStrLn $ "importing done " ++ show nm
      modify $ \ c -> c { working = tail (working c), cache = M.insert nm cm (cache c) }
      pure cm
    Just cm -> do
      when (verbose flags) $
        liftIO $ putStrLn $ "importing cached " ++ show nm
      pure cm

compileModule :: Flags -> IdentModule -> StateT Cache IO Module
compileModule flags nm = do
  let fn = map (\ c -> if c == '.' then '/' else c) nm ++ ".hs"
  mdl@(EModule nm' _ defs) <- parseDie pTop fn <$> liftIO (readFilePath (paths flags) fn)
  when (nm /= nm') $
    error $ "module name does not agree with file name: " ++ show nm
  let specs = [ s | Import s <- defs ]
  impMdls <- mapM (compileModuleCached flags) [ m | ImportSpec _ m _ <- specs ]
  pure $ desugar (zip specs impMdls) mdl

------------------

readFilePath :: [FilePath] -> String -> IO String
readFilePath path name = hGetContents =<< openFilePath path name

tryOpenFile :: FilePath -> IO (Either IOError Handle)
tryOpenFile path = try $ openFile path ReadMode

openFilePath :: [FilePath] -> String -> IO Handle
openFilePath [] fileName = error $ "File not found: " ++ show fileName
openFilePath (dir:dirs) fileName = do
    let path = dir ++ "/" ++ fileName
    result <- tryOpenFile path
    case result of
        Left _ -> openFilePath dirs fileName -- If opening failed, try the next directory
        Right hdl -> return hdl
