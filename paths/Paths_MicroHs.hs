-- This module is used when we are not compiling with cabal/mcabal.
module Paths_MicroHs(
  version,
  getDataDir,
  ) where
import Data.Version

getDataDir :: IO FilePath
getDataDir = return "."

version :: Version
version = makeVersion [0,9,16,0]
