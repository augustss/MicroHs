module MicroHs.Flags(Flags(..), verbosityGT, defaultFlags) where

data Flags = Flags {
  verbose    :: Int,        -- verbosity level
  runIt      :: Bool,       -- run instead of compile
  mhsdir     :: FilePath,   -- where MHS files live
  paths      :: [FilePath], -- module search path
  output     :: String,     -- output file
  loading    :: Bool,       -- show loading message
  readCache  :: Bool,       -- read and use cache
  writeCache :: Bool,       -- generate cache
  useTicks   :: Bool,       -- emit ticks
  doCPP      :: Bool,       -- run ccphs on input files
  cppArgs    :: [String],   -- flags for CPP
  compress   :: Bool,       -- compress generated combinators
  buildPkg   :: Maybe FilePath, -- build a package
  pkgPath    :: [FilePath]  -- package search path
  }
  --deriving (Show)

verbosityGT :: Flags -> Int -> Bool
verbosityGT flags v = verbose flags > v

defaultFlags :: FilePath -> Flags
defaultFlags dir = Flags {
  verbose    = 0,
  runIt      = False,
  mhsdir     = dir,
  paths      = [".", dir ++ "/lib"],
  output     = "out.comb",
  loading    = False,
  readCache  = False,
  writeCache = False,
  useTicks   = False,
  doCPP      = False,
  cppArgs    = [],
  compress   = False,
  buildPkg   = Nothing,
  pkgPath    = []
  }
