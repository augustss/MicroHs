-- This module is used when we are not compiling with cabal/mcabal.
module Paths_MicroHs(
  version,
  getDataDir,
  ) where
import Data.Version

version :: Version
version = makeVersion [0,9,15,0]

getDataDir :: IO FilePath
getDataDir = return "."
