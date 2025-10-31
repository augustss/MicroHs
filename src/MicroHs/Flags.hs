module MicroHs.Flags(
  Flags(..), verbosityGT, defaultFlags,
  DumpFlag(..), dumpIf,
  wantGMP) where
import qualified Prelude(); import MHSPrelude

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
  lArgs      :: [String],   -- arguments for C linker
  compress   :: Bool,       -- compress generated combinators
  base64     :: Bool,       -- base64 encode generated combinators
  buildPkg   :: Maybe FilePath, -- build a package
  listPkg    :: Maybe FilePath, -- list package contents
  pkgPath    :: [FilePath], -- package search path
  installPkg :: Bool,       -- install a package
  preload    :: [String],   -- packages to preload
  target     :: String,     -- Compile target defined in target.conf
  dumpFlags  :: [DumpFlag], -- For debugging,
  useStdin   :: Bool,       -- Use stdin in interactive system
  noLink     :: Bool        -- Just generate an unlinked object file
  }
  deriving (Show)

verbosityGT :: Flags -> Int -> Bool
verbosityGT flags v = verbose flags > v

defaultFlags :: FilePath -> Flags
defaultFlags dir = Flags {
  verbose    = 0,
  runIt      = False,
  mhsdir     = dir,
  paths      = ["."] ++ gmp ++ [dir ++ "/lib"],
  output     = "out.comb",
  loading    = False,
  speed      = False,
  readCache  = False,
  writeCache = False,
  useTicks   = False,
  doCPP      = False,
  cppArgs    = [],
  cArgs      = [],
  lArgs      = [],
  compress   = False,
  base64     = False,
  buildPkg   = Nothing,
  listPkg    = Nothing,
  pkgPath    = [],
  installPkg = False,
  preload    = [],
  target     = "default",
  dumpFlags  = [],
  useStdin   = False,
  noLink     = False
  }
  -- This is a hack so that the in-place mhs picks up GMP.
  where gmp | dir == "." && wantGMP = ["lib/gmp"]
            | otherwise             = []

data DumpFlag = Dpreproc | Dparse | Dderive | Dtypecheck | Ddesugar | Dlinked | Dtoplevel | Dcombinator | Dall
  deriving (Eq, Show, Enum, Bounded)

dumpIf :: Monad m => Flags -> DumpFlag -> m () -> m ()
dumpIf flags df act | df `elem` dfs || Dall `elem` dfs = act
                    | otherwise = return ()
  where dfs = dumpFlags flags
