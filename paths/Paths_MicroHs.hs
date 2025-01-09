-- This module is used when we are not compiling with cabal/mcabal.
module Paths_MicroHs(
  version,
  getDataDir,
  ) where
import Prelude(); import MHSPrelude
import Data.Version

getDataDir :: IO FilePath
getDataDir = return "."

version :: Version
version = makeVersion [0,11,1,10]
