module System.Console.SimpleReadline(
  getInputLine,
  getInputLineHist
  ) where

getInputLine :: String -> IO (Maybe String)
getInputLine _ = error "No getInputLine for ghc"

getInputLineHist :: FilePath -> String -> IO (Maybe String)
getInputLineHist _ _ = error "No getInputLineHist for ghc"
