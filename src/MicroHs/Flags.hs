module MicroHs.Flags(Flags(..), verbosityGT, defaultFlags) where
import Prelude(); import MHSPrelude

data Flags = Flags {
  verbose    :: Int,        -- verbosity level
  runIt      :: Bool,       -- run instead of compile
  mhsdir     :: FilePath,   -- where MHS files live
  paths      :: [FilePath], -- module search path
  output     :: String,     -- output file
  loading    :: Bool,       -- show loading message
  speed      :: Bool,       -- show lines/s
  readCache  :: Bool,       -- read and use cache
  writeCache :: Bool,       -- generate cache
  useTicks   :: Bool,       -- emit ticks
  doCPP      :: Bool,       -- run ccphs on input files
  cppArgs    :: [String],   -- flags for CPP
  cArgs      :: [String],   -- arguments for C compiler
  compress   :: Bool,       -- compress generated combinators
  buildPkg   :: Maybe FilePath, -- build a package
  listPkg    :: Maybe FilePath, -- list package contents
  pkgPath    :: [FilePath], -- package search path
  installPkg :: Bool,       -- install a package
  target     :: String      -- Compile target defined in target.conf
  }
  deriving (Show)

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
  speed      = False,
  readCache  = False,
  writeCache = False,
  useTicks   = False,
  doCPP      = False,
  cppArgs    = [],
  cArgs      = [],
  compress   = False,
  buildPkg   = Nothing,
  listPkg    = Nothing,
  pkgPath    = [],
  installPkg = False,
  target     = "default"
  }
