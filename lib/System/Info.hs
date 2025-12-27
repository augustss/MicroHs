module System.Info(os, arch, compilerName, compilerVersion, fullCompilerVersion) where
import Data.Char
import Data.Version
import System.Process
import System.IO.Unsafe

os :: String
os = if _isWindows then "windows" else uname "-s"

arch :: String
arch = if _isWindows then "x86_64" else
  case uname "-m" of
    "arm64" -> "aarch64"     -- match ghc
    s       -> s

compilerName :: String
compilerName = "mhs"

compilerVersion :: Version
compilerVersion = fullCompilerVersion { versionBranch = take 2 (versionBranch fullCompilerVersion) }

-- Assume the system has an uname command.
uname :: String -> String
uname flag = unsafePerformIO $ do
  res <- readProcess "uname" [flag] ""
  return $ map toLower $ filter (not . isSpace) res

fullCompilerVersion :: Version
fullCompilerVersion = makeVersion [0,15,2,0]
