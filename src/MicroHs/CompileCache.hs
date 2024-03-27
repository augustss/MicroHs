module MicroHs.CompileCache(
  CModule(..), tModuleOf,
  Cache, cache, addWorking, emptyCache, deleteFromCache, workToDone,
  saveCache, loadCached,
  ) where
import Prelude
import MicroHs.Desugar(LDef)
import MicroHs.Expr(IdentModule)
import MicroHs.Ident(showIdent)
import qualified MicroHs.IdentMap as M
import MicroHs.TypeCheck(TModule)
import System.IO
import System.IO.Serialize
import System.IO.MD5(MD5CheckSum)
import Compat

data CModule = CModule
    (TModule [LDef])                    -- the cached module
    [IdentModule]                       -- imported module names
    MD5CheckSum                         -- checksum of the source file
--  deriving (Show)

tModuleOf :: CModule -> TModule [LDef]
tModuleOf (CModule t _ _) = t

data Cache = Cache {
  working :: [IdentModule],             -- modules currently being processed (used to detected circular imports)
  cache   :: M.Map CModule              -- cached compiled modules
  }
--  deriving (Show)

emptyCache :: Cache
emptyCache = Cache [] M.empty

deleteFromCache :: IdentModule -> Cache -> Cache
deleteFromCache mn (Cache is m) = Cache is (M.delete mn m)

addWorking :: IdentModule -> Cache -> Cache
addWorking mn c =
  let ws = working c
  in  if elem mn ws then
        error $ "recursive module: " ++ showIdent mn ++ ", import chain: " ++ unwords (map showIdent ws)
      else
        c{ working = mn : ws }

workToDone :: CModule -> Cache -> Cache
workToDone cm (Cache (mn:ws) m) = Cache ws (M.insert mn cm m)
workToDone _ _ = undefined

saveCache :: FilePath -> Cache -> IO ()
saveCache fn cash = writeSerializedCompressed fn cash

loadCached :: FilePath -> IO (Maybe Cache)
loadCached fn = do
  mhin <- openFileM fn ReadMode
  case mhin of
    Nothing ->
      return Nothing
    Just hin -> do
      hClose hin
      Just <$> readSerialized fn
