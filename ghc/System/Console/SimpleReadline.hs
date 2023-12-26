module System.Console.SimpleReadline(
  getInputLine,
  getInputLineHist
  ) where
import qualified System.Console.Haskeline as H
import System.Console.Haskeline hiding (getInputLine)

getInputLine :: String -> IO (Maybe String)
getInputLine prompt =
  runInputT defaultSettings (H.getInputLine prompt)

getInputLineHist :: FilePath -> String -> IO (Maybe String)
getInputLineHist hist prompt =
  runInputT settings (H.getInputLine prompt)
  where settings = defaultSettings { historyFile = Just hist }
