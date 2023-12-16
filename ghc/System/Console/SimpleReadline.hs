module System.Console.SimpleReadline(
  getInputLine,
  getInputLineHist
  ) where
import qualified System.Console.Haskeline as H

getInputLine :: String -> IO (Maybe String)
getInputLine prompt =
  H.runInputT H.defaultSettings (H.getInputLine prompt)

getInputLineHist :: FilePath -> String -> IO (Maybe String)
getInputLineHist hist prompt =
  H.runInputT settings (H.getInputLine prompt)
  where settings = H.defaultSettings { H.historyFile = Just hist }
