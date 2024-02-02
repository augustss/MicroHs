module System.Exit(
  ExitCode(..),
  exitWith,
  exitFailure,
  exitSuccess,
  die,
  ) where
import Prelude
import Control.Exception
import System.IO

data ExitCode = ExitSuccess | ExitFailure Int
  deriving (Show)

-- XXX This needs work
exitWith :: forall a . ExitCode -> IO a
exitWith e = throwIO (Exn (show e))

exitFailure :: forall a . IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: forall a . IO a
exitSuccess = exitWith ExitSuccess

die :: forall a . String -> IO a
die err = hPutStrLn stderr err >> exitFailure
