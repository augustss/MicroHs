-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.Environment(module System.Environment) where
import Primitives
import Data.Char  -- for String

getArgs :: IO [String]
getArgs = primGetArgs
