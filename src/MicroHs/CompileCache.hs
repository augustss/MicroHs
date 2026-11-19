module MicroHs.CompileCache(
  CModule,
  Cache, addWorking, getWorking, emptyCache, deleteFromCache, workToDone, addBoot, getBoots,
  cachedModules, cachedModuleNames, cachedNonPkgModuleNames,
  lookupCache, lookupCacheChksum, getImportDeps,
  addPackage, getCompMdls, getPathPkgs, getPkgs, getEmbedPkgs,
  getCacheTables, setCacheTables,
  saveCache, loadCached, LoadedCache(..),
  ) where
import qualified Prelude(); import MHSPrelude
import qualified Data.ByteString.Char8 as BS
import MicroHs.Desugar(LDef)
import MicroHs.Error(mhsErrorLoc)
import MicroHs.Expr(IdentModule, getSLoc)
import MicroHs.Ident(showIdent)
import qualified MicroHs.IdentMap as M
import MicroHs.Package
import MicroHs.TypeCheck(TModule(..), GlobTables, emptyGlobTables, mergeGlobTables, deleteModuleGlobTables, isInstId)
import System.IO
import System.IO.Serialize
import System.IO.MD5(MD5CheckSum)

type CModule = (TModule [LDef], [IdentModule], MD5CheckSum)

data CacheEntry =
   CompMdl                              -- module compiled in in this session
    (TModule [LDef])                    -- the cached module
    [IdentModule]                       -- imported module names
    MD5CheckSum                         -- checksum of the source file
  | PkgMdl                              -- module from a package
    (TModule [LDef])                    -- the cached module
--  deriving (Show)

instance NFData CacheEntry where
  rnf (CompMdl a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (PkgMdl a) = rnf a

tModuleOf :: CacheEntry -> TModule [LDef]
tModuleOf (CompMdl t _ _) = t
tModuleOf (PkgMdl t) = t

chksumOf :: CacheEntry -> MD5CheckSum
chksumOf (CompMdl _ _ k) = k
chksumOf _ = undefined

data Cache = Cache {
  working :: [IdentModule],             -- modules currently being processed (used to detected circular imports)
  boots   :: [IdentModule],             -- modules where only the boot version has been compiled
  cache   :: M.Map CacheEntry,          -- cached compiled modules
  others  :: [TModule [LDef]],          -- invisible package modules
  pkgs    :: [(Maybe FilePath, Package)], -- loaded packages, no filename for embedded
  tables  :: GlobTables                 -- glbals, e.g., instances
  }
--  deriving (Show)

instance NFData Cache where
  rnf (Cache a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

forceCache :: Cache -> Cache
forceCache c = force c

getCacheTables :: Cache -> GlobTables
getCacheTables = tables

setCacheTables :: GlobTables -> Cache -> Cache
setCacheTables ct c = c{ tables = ct }

emptyCache :: Cache
emptyCache =
  Cache { working = [], boots = [], cache = M.empty, others = [], pkgs = [], tables = emptyGlobTables }

-- Remove a module from the cache, and what it added to the global tables,
-- so that, e.g., its instances are not used by the modules compiled after it.
deleteFromCache :: IdentModule -> Cache -> Cache
deleteFromCache mn c =
  case M.lookup mn (cache c) of
    Nothing -> c
    Just e ->
      let insts = [ i | (i, _) <- tBindingsOf (tModuleOf e), isInstId i ]
      in  c{ cache = M.delete mn (cache c), tables = deleteModuleGlobTables mn insts (tables c) }

addBoot :: IdentModule -> Cache -> Cache
addBoot mn c = c{ boots = mn : boots c }

getBoots :: Cache -> [IdentModule]
getBoots = boots

getWorking :: Cache -> [IdentModule]
getWorking = working

addWorking :: IdentModule -> Cache -> Cache
addWorking mn c =
  let ws = working c
  in  if mn `elem` ws then
        mhsErrorLoc (getSLoc mn) $ "recursive module: " ++ showIdent mn ++ ", import chain: " ++ unwords (map showIdent ws)
      else
        c{ working = mn : ws }

workToDone :: CModule -> Cache -> Cache
workToDone (t, i, k) c@(Cache{ working = _mn:ws, boots = bs, cache = m }) =
  c{ working = ws, boots = filter (/= mn) bs, cache = M.insert mn (CompMdl t i k) m }
  -- The identifier in _mn is from the place of use (=import), but we want the cache
  -- to contain the identifier from the place of definition.
  where mn = tModuleName t
workToDone _ _ = undefined

cachedModules :: Cache -> [TModule [LDef]]
cachedModules c = others c ++ (map tModuleOf . M.elems . cache) c

cachedModuleNames :: Cache -> [IdentModule]
cachedModuleNames = M.keys . cache

cachedNonPkgModuleNames :: Cache -> [IdentModule]
cachedNonPkgModuleNames c = [ i | (i, CompMdl{}) <- M.toList (cache c) ]

lookupCache :: IdentModule -> Cache -> Maybe (TModule [LDef])
lookupCache mn c = tModuleOf <$> M.lookup mn (cache c)

lookupCacheChksum :: IdentModule -> Cache -> Maybe MD5CheckSum
lookupCacheChksum mn c = chksumOf <$> M.lookup mn (cache c)

getImportDeps :: Cache -> [(IdentModule, [IdentModule])]
getImportDeps cash = [ (tModuleName tm, imps) | CompMdl tm imps _ <- M.elems (cache cash) ]

getCompMdls :: Cache -> [TModule [LDef]]
getCompMdls cash = [ tm | CompMdl tm _ _ <- M.elems (cache cash) ]

getPathPkgs :: Cache -> [(FilePath, Package)]
getPathPkgs ch = [ (fp, p) | (Just fp, p) <- pkgs ch ]

getPkgs :: Cache -> [Package]
getPkgs = map snd . pkgs

getEmbedPkgs :: Cache -> [Package]
getEmbedPkgs ch = [ p | (Nothing, p) <- pkgs ch ]

addPackage :: Maybe FilePath -> Package -> Cache -> Cache
addPackage mf p c = c{
  pkgs = (mf, p) : pkgs c,
  cache = foldr ins (cache c) (pkgExported p),
  others = pkgOther p ++ others c,
  tables = mergeGlobTables (pkgTables p) (tables c)
  }
  where ins t = M.insert (tModuleName t) (PkgMdl t)

-- The cache file starts with a line that says which compiler wrote it,
-- followed by the serialized cache.  Deserializing does no type checking,
-- so a cache written by another compiler, where the types can differ,
-- must not be read.
-- Change cacheFormat when the layout of the cache changes.
cacheFormat :: Int
cacheFormat = 1

-- The first line of the cache file, for a compiler version.
cacheHeader :: String -> BS.ByteString
cacheHeader version = BS.pack $ "MicroHs cache, compiler " ++ version ++ ", format " ++ show cacheFormat ++ "\n"

saveCache :: String -> FilePath -> Cache -> IO ()
saveCache version fn cash = do
  bs <- writeSerializedCompressedBS (forceCache cash)
  h <- openBinaryFile fn WriteMode
  BS.hPut h (cacheHeader version)
  BS.hPut h bs
  hClose h

data LoadedCache
  = NoCacheFile                         -- there is no cache file
  | OtherCompiler                       -- the cache was written by another compiler
  | LoadedCache Cache

loadCached :: String -> FilePath -> IO LoadedCache
loadCached version fn = do
  mhin <- openBinaryFileM fn ReadMode
  case mhin of
    Nothing ->
      return NoCacheFile
    Just hin -> do
      bs <- BS.hGetContents hin
      let hdr = cacheHeader version
      if hdr `BS.isPrefixOf` bs then
        LoadedCache <$> readSerializedBS (BS.drop (BS.length hdr) bs)
      else
        return OtherCompiler
