-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Instances(compiledWithGHC) where
-- For GHC compatibility
import Prelude
import System.Environment

compiledWithGHC :: Bool
compiledWithGHC = False
