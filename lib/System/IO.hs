-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO(
  module System.IO.Base,
  readIO,
  readLn,
  fixIO, FixIOException,
  Newline(..),
  NewlineMode(..),
  ) where
import qualified Prelude()              -- do not import Prelude
import MiniPrelude
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Fix
import Mhs.Builtin
import System.IO.Base
import System.IO.Error
import System.IO.Unsafe(unsafeInterleaveIO)
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

------------------------------------------------

data FixIOException = FixIOException

instance Exception FixIOException

instance Show FixIOException where
  showsPrec _ FixIOException = showString "cyclic evaluation in fixIO"

fixIO :: (a -> IO a) -> IO a
fixIO k = do
    m <- newEmptyMVar
    ans <- unsafeInterleaveIO
             (readMVar m `catch` \ BlockedIndefinitelyOnMVar ->
                                   throwIO FixIOException)
    result <- k ans
    putMVar m result
    return result

instance MonadFix IO where
  mfix = fixIO

data Newline = LF | CRLF
  deriving (Eq, Ord, Show, Read)

data NewlineMode = NewlineMode {inputNL :: Newline, outputNL :: Newline}
  deriving (Eq, Ord, Show, Read)
