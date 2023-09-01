-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.Environment(module System.Environment) where
import Primitives
--import Data.Char  -- for String
import System.IO --Y()

getArgs :: IO [[Char]]
getArgs = primGetArgs

withDropArgs :: forall a . Int -> IO a -> IO a
withDropArgs = primWithDropArgs
