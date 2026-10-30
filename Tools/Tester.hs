module Tester(main) where
import Control.Exception
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
  let (args', ems) =
        case args of
          "-temscripten":r -> (r, True)
          _ -> (args, False)
  file <- case args of
    [f] -> return f
    _   -> error "Usage: Tester [-temscripten] <test-file>"
  tmhs <- fromMaybe "../bin/mhs +RTS -H10M -RTS -CR" <$> lookupEnv "TMHS"
  content <- readFile file
  let specs = parseTests (lines content)
  mapM_ (runOne tmhs ems) specs
  removeFileM tmpIn
  removeFileM tmpOut
  removeFileM tmpErr

tmpIn :: FilePath
tmpIn = "tester.in"

tmpOut :: FilePath
tmpOut = "tester.out"

tmpErr :: FilePath
tmpErr = "tester.err"

removeFileM :: FilePath -> IO ()
removeFileM fn = catch (removeFile fn) (\ (_ :: SomeException) -> return ())

type TestSpec = (String, FilePath, FilePath, FilePath, [String])

parseTests :: [String] -> [TestSpec]
parseTests = mapMaybe parseLine . zip [1 :: Int ..]
  where
    parseLine (n, line) =
      case words (takeWhile (/= '#') line) of
        []                                -> Nothing
        prog : []                         -> Just (prog, "-",  prog ++ ".ref" , "-",      ["-r"])
        prog : oref : eref : iref : flags -> Just (prog, iref, sub oref,        sub eref, flags)
          where sub "=" = prog ++ ".ref"
                sub s   = s
        _                                 -> error $ "Bad test specification on line " ++ show n ++ ": " ++ line

readFileClean :: FilePath -> IO String
readFileClean fn = do
  s <- filter (/= '\r') <$> readFile fn
  seq (length s) (return s)

readRef :: FilePath -> IO String
readRef "-" = return ""
readRef fn  = readFileClean fn

runOne :: String -> Bool -> TestSpec -> IO ()
runOne tmhs ems (prog, inRef, outRef, errRef, flags) = do
  let --emsflag = if ems then " -temscripten -oout.js" else ""
      cmd = [tmhs, "-a"] ++ flags ++ [prog]
      cmd' = unwords $ cmd ++ ["<", tmpIn, ">", tmpOut, "2>", tmpErr]
  infile <- readRef inRef
  writeFile tmpIn infile
  putStrLn $ unwords cmd
  hFlush stdout
  rc <- system cmd'
--  when ems $
--    system $ "node out.js" ++ " > " ++ tmpOut
  actOut <- readFileClean tmpOut
  actErr <- readFileClean tmpErr
  expOut <- readRef outRef
  expErr <- readRef errRef
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
