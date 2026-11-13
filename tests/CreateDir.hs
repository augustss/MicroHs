module CreateDir(main) where
import Control.Exception
import System.Directory

-- Print whether the action raised an IOException.
check :: String -> IO () -> IO ()
check msg act = do
  r <- try act
  putStrLn $ msg ++ ": " ++ either (\ (e :: IOException) -> "error") (const "ok") r

-- Everything goes under cdtest.tmp (ignored by git), which is left
-- behind like the other .tmp files; a rerun finds it already there.
main :: IO ()
main = do
  check "nested" $ createDirectoryIfMissing True "cdtest.tmp/a/b"
  doesDirectoryExist "cdtest.tmp/a/b" >>= print
  check "nested again" $ createDirectoryIfMissing True "cdtest.tmp/a/b"
  check "trailing slash" $ createDirectoryIfMissing True "cdtest.tmp/a/c/"
  check "existing" $ createDirectoryIfMissing False "cdtest.tmp/a"
  check "missing parent" $ createDirectoryIfMissing False "cdtest.tmp/x/y"
  writeFile "cdtest.tmp/f" ""
  check "file in the way" $ createDirectoryIfMissing True "cdtest.tmp/f/sub"
  check "file at target" $ createDirectoryIfMissing False "cdtest.tmp/f"
  cwd <- getCurrentDirectory
  check "empty" $ createDirectoryIfMissing True ""
  check "absolute" $ createDirectoryIfMissing True (cwd ++ "/cdtest.tmp/d/e")
  doesDirectoryExist "cdtest.tmp/d/e" >>= print
