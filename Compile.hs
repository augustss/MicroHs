module Compile where
import Control.Exception
import Control.Monad.State.Strict
import qualified Data.Map as M
import System.IO
import Desugar
import Exp
import Parse

data Flags = Flags {
  verbose :: Bool,
  path :: [FilePath]
  }
  deriving (Show)

type SymTable = M.Map Ident [Exp]

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
  mdl@(EModule nm' defs) <- parseDie pTop fn <$> liftIO (readFilePath (path flags) fn)
  when (nm /= nm') $
    error $ "module name does not agree with file name: " ++ show nm
  impMdls <- mapM (compileModuleCached flags) [ m | Import m <- defs ]
  let
    mdl' :: Module
    mdl' = desugar mdl
    allSyms :: SymTable
    allSyms = M.fromListWith (++) $ concatMap syms (mdl' : impMdls)
    syms (Module mn qis _ _) = [ (v, [Var qi]) | (i, qi) <- qis, v <- [i, qual mn i] ]
    mdl'' = checkModule allSyms mdl'
--  liftIO $ print allSyms
  pure mdl''

checkModule :: SymTable -> Module -> Module
checkModule syms (Module nm exps tds ds) = Module nm exps tds [ (s, checkExpr syms e) | (s, e) <- ds ]

checkExpr :: SymTable -> Exp -> Exp
checkExpr syms ee =
  case ee of
    Var i ->
      case M.lookup i syms of
        Nothing -> error $ "undefined: " ++ show i
        Just [qi] -> qi
        Just qis -> error $ "ambiguous: " ++ show i ++ ", " ++ show qis
    App e1 e2 -> App (checkExpr syms e1) (checkExpr syms e2)
    Lam i e -> Lam i (checkExpr syms' e) where syms' = M.insertWith (++) i [Var i] syms
    Lbl _ _ -> undefined
    e -> e

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
        Right handle -> return handle
