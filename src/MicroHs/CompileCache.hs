module MicroHs.CompileCache(
  module MicroHs.CompileCache
  ) where
import Prelude
import MicroHs.Desugar(LDef)
import MicroHs.Expr(IdentModule)
--import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.TypeCheck(TModule)

type CModule = TModule [LDef]
data Cache = Cache [IdentModule] (M.Map CModule)
  deriving (Show)

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

