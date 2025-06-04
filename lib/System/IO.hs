-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO(
  module System.IO.Base,
  readIO,
  readLn,
  ) where
import qualified Prelude()              -- do not import Prelude
import MiniPrelude
import Mhs.Builtin
import System.IO.Base
import System.IO.Error
import Text.Read

readLn :: Read a => IO a
readLn = getLine >>= readIO

readIO :: Read a => String -> IO a
readIO s =
  case (do { (x,t) <- reads s;
             ("","") <- lex t;
             return x }) of
    [x]    -> return x
    []     -> ioError (userError "Prelude.readIO: no parse")
    _      -> ioError (userError "Prelude.readIO: ambiguous parse")
