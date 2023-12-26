module MicroHs.CompileCache(
  CModule(..), tModuleOf,
  Cache, cache, working, updWorking, emptyCache, deleteFromCache, workToDone,
  ) where
import Prelude
import MicroHs.Desugar(LDef)
import MicroHs.Expr(IdentModule)
--import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.TypeCheck(TModule)
import System.IO.MD5(MD5CheckSum)

data CModule = CModule
    (TModule [LDef])                    -- the cached module
    [IdentModule]                       -- imported module names
    MD5CheckSum                         -- checksum of the source file
--  deriving (Show)

tModuleOf :: CModule -> TModule [LDef]
tModuleOf (CModule t _ _) = t

data Cache = Cache [IdentModule] (M.Map CModule)
--  deriving (Show)

working :: Cache -> [IdentModule]
working (Cache x _) = x

updWorking :: [IdentModule] -> Cache -> Cache
updWorking w (Cache _ m) = Cache w m

cache :: Cache -> M.Map CModule
cache (Cache _ x) = x

emptyCache :: Cache
emptyCache = Cache [] M.empty

deleteFromCache :: IdentModule -> Cache -> Cache
deleteFromCache mn (Cache is m) = Cache is (M.delete mn m)

workToDone :: CModule -> Cache -> Cache
workToDone cm (Cache (mn:ws) m) = Cache ws (M.insert mn cm m)
workToDone _ _ = undefined
