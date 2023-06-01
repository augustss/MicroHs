module Compile where
import Control.Monad.State.Strict
import qualified Data.Map as M
import Desugar
import Exp
import Parse

type IdentModule = Ident
type CModule = (IdentModule, [LDef])
type SymTable = M.Map Ident [Exp]

data Cache = Cache
  { working :: [IdentModule]
  , cache   :: M.Map IdentModule CModule
  }
  deriving (Show)

compile :: IdentModule -> IO [LDef]
compile nm = do
  ch <- execStateT (compileModuleCached nm) Cache{ working = [], cache = M.empty }
  let qual (mn, ds) = [(mn ++ "." ++ i, e) | (i, e) <- ds ]
  pure $ concatMap qual $ M.elems $ cache ch

compileModuleCached :: IdentModule -> StateT Cache IO CModule
compileModuleCached nm = do
  ch <- gets cache
  case M.lookup nm ch of
    Nothing -> do
      ws <- gets working
      when (nm `elem` ws) $
        error $ "recursive module: " ++ show nm
      modify $ \ c -> c { working = nm : working c }
      liftIO $ putStrLn $ "importing " ++ show nm
      cm <- compileModule nm
      liftIO $ putStrLn $ "importing done " ++ show nm
      modify $ \ c -> c { working = tail (working c), cache = M.insert nm cm (cache c) }
      pure cm
    Just cm -> do
      liftIO $ putStrLn $ "importing cached " ++ show nm
      pure cm

compileModule :: IdentModule -> StateT Cache IO CModule
compileModule nm = do
  let fn = nm ++ ".uhs"
  mdl@(Module nm' defs) <- parseDie pTop fn <$> liftIO (readFile fn)
  when (nm /= nm') $
    error $ "module name does not agree with file name: " ++ show nm
  impMdls :: [CModule]
          <- mapM compileModuleCached [ m | Import m <- defs ]
  let
    mdl' :: CModule
    mdl' = desugar mdl
    allSyms :: SymTable
    allSyms = M.fromListWith (++) $ concatMap syms (mdl' : impMdls)
    syms (mn, ds) = [ (s, [Var qi]) | (i, _) <- ds, let qi = mn ++ "." ++ i, s <- [i, qi] ]
    mdl'' = checkModule allSyms mdl'
--  liftIO $ print allSyms
  pure mdl''

checkModule :: SymTable -> CModule -> CModule
checkModule syms (nm, ds) = (nm, [ (s, checkExpr syms e) | (s, e) <- ds ])

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
