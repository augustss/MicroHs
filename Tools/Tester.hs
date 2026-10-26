module Tester(main) where
import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
  args <- getArgs
  file <- case args of
    [f] -> return f
    _   -> error "Usage: Tester <test-file>"
  tmhs <- fromMaybe "../bin/mhs +RTS -H10M -RTS -CR" <$> lookupEnv "TMHS"
  content <- readFile file
  let specs = parseTests (lines content)
  mapM_ (runOne tmhs) specs
  removeFileM tmpOut
  removeFileM tmpErr

tmpOut :: FilePath
tmpOut = "tester.out"

tmpErr :: FilePath
tmpErr = "tester.err"

removeFileM :: FilePath -> IO ()
removeFileM fn = do
  b <- doesFileExist fn
  when b $ removeFile fn

-- (program, Maybe stdoutRef, Maybe stderrRef)
-- If stdoutRef is Nothing, only compile (no -r) and expect empty stdout and stderr.
-- A ref file of "-" means empty output is expected.
type TestSpec = (String, Maybe FilePath, Maybe FilePath)

parseTests :: [String] -> [TestSpec]
parseTests = mapMaybe parseLine . zip [1 :: Int ..]
  where
    parseLine (n, line) =
      case words (takeWhile (/= '#') line) of
        []                 -> Nothing
        [prog]             -> Just (prog, Nothing, Nothing)
        [prog, oref]       -> Just (prog, Just oref, Nothing)
        [prog, oref, eref] -> Just (prog, Just oref, Just eref)
        _                  -> error $ "Bad test specification on line " ++ show n ++ ": " ++ line

readFileClean :: FilePath -> IO String
readFileClean fn = do
  s <- filter (/= '\r') <$> readFile fn
  seq (length s) (return s)

readRef :: Maybe FilePath -> IO String
readRef Nothing    = return ""
readRef (Just "-") = return ""
readRef (Just fn)  = readFileClean fn

runOne :: String -> TestSpec -> IO ()
runOne tmhs (prog, mOutRef, mErrRef) = do
  let rflag = case mOutRef of
                Nothing -> ""
                Just _  -> " -r"
      cmd = tmhs ++ rflag ++ " " ++ prog ++ " > " ++ tmpOut ++ " 2> " ++ tmpErr
  putStrLn $ tmhs ++ rflag ++ " " ++ prog
  hFlush stdout
  rc <- system cmd
  actOut <- readFileClean tmpOut
  actErr <- readFileClean tmpErr
  expOut <- readRef mOutRef
  expErr <- readRef mErrRef
  when (null expErr && rc /= ExitSuccess) $ do
    putStrLn $ "FAILED (exit code " ++ show rc ++ "): " ++ prog
    unless (null actOut) $ putStrLn $ "stdout:\n" ++ actOut
    unless (null actErr) $ putStrLn $ "stderr:\n" ++ actErr
    exitFailure
  when (actOut /= expOut) $ do
    putStrLn $ "FAILED stdout mismatch: " ++ prog
    putStrLn $ "expected:\n" ++ expOut
    putStrLn $ "actual:\n" ++ actOut
    exitFailure
  when (actErr /= expErr) $ do
    putStrLn $ "FAILED stderr mismatch: " ++ prog
    putStrLn $ "expected:\n" ++ expErr
    putStrLn $ "actual:\n" ++ actErr
    exitFailure
