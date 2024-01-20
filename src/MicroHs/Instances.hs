-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Instances(getMhsDir, compiledWithGHC) where
-- For GHC compatibility
import Prelude
import System.Environment

getMhsDir :: IO (Maybe FilePath)
getMhsDir = lookupEnv "MHSDIR"

compiledWithGHC :: Bool
compiledWithGHC = False
