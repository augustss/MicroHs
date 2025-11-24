module System.Info(os, arch, compilerName, compilerVersion, fullCompilerVersion) where
import Control.Monad
import Data.Char
import Data.Version
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
compilerVersion = fullCompilerVersion { versionBranch = take 2 (versionBranch fullCompilerVersion) }

-- Assume the system has an uname command
uname :: String -> String
uname flag = unsafePerformIO $ do
  print "A"
  (fn, h) <- openTmpFile "uname"
  print "B"
  hClose h
  print "C"
  rc <- system $ "uname " ++ flag ++ " >" ++ fn
  print "D"
  res <- readFile fn
  print "E"
  removeFile fn
  print "F"
  when (rc /= ExitSuccess) $
    error "System.Into: uname failed"
  print "G"
  return $ map toLower $ filter (not . isSpace) res

fullCompilerVersion = makeVersion [0,14,25,0]
