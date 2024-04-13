module System.Info(os, arch, compilerName, compilerVersion, fullCompilerVersion) where
import Data.Char
import Data.Version(Version(..))
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.IO.Unsafe

os :: String
os = if _isWindows then "windows" else uname "-s"

arch :: String
arch = if _isWindows then "x86_64" else uname "-m"

compilerName :: String
compilerName = "mhs"

compilerVersion :: Version
compilerVersion = Version [0,9]

fullCompilerVersion :: Version
fullCompilerVersion = Version [0,9,0]

-- Assume the system has an uname command
uname :: String -> String
uname flag = unsafePerformIO $ do
  (fn, h) <- openTmpFile "uname"
  hClose h
  rc <- cmd $ "uname " ++ flag ++ " >" ++ fn
  res <- readFile fn
  removeFile fn
  when (rc /= ExitSuccess)
    error $ "System.Into: uname failed"
  return $ map toLower $ filter (not . isSpace) res
