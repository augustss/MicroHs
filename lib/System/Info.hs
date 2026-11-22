module System.Info(os, arch, compilerName, compilerVersion, fullCompilerVersion) where
import Data.Char
import Data.Version
import System.Process
import System.IO.Unsafe

-- The names of the operating system and the architecture are those that
-- GHC uses, except that Windows is "windows" unless the runtime was
-- compiled with MinGW.  They also give the CPP symbols os_HOST_OS and
-- arch_HOST_ARCH.
os :: String
os = if _isWindows then (if isMinGW then "mingw32" else "windows") else
  case uname "-s" of
    "sunos" -> "solaris2"
    s       -> s

arch :: String
arch = if _isWindows then if _wordSize == 64 then "x86_64" else "i386" else
  case uname "-m" of
    "arm64"   -> "aarch64"
    "amd64"   -> "x86_64"
    "ppc64le" -> "powerpc64le"
    "ppc64"   -> "powerpc64"
    "ppc"     -> "powerpc"
    s | s `elem` ["i386", "i486", "i586", "i686", "x86"] -> "i386"
      | take 3 s == "arm" -> "arm"
      | otherwise -> s

compilerName :: String
compilerName = "mhs"

compilerVersion :: Version
compilerVersion = fullCompilerVersion { versionBranch = take 2 (versionBranch fullCompilerVersion) }

foreign import ccall "ismingw" c_ismingw :: IO Int
isMinGW :: Bool
isMinGW = unsafePerformIO c_ismingw == 1

-- Assume the system has an uname command.
uname :: String -> String
uname flag = unsafePerformIO $ do
  res <- readProcess "uname" [flag] ""
  return $ map toLower $ filter (not . isSpace) res

fullCompilerVersion :: Version
fullCompilerVersion = makeVersion [0,17,2,0]
