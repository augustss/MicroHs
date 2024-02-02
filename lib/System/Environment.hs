-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.Environment(
  getArgs,
  getProgName,
  withArgs,
  lookupEnv,
  ) where
import Prelude
import Primitives(primPerformIO, primGetArgs)
import Data.IORef
import Foreign.C.String
import Foreign.Ptr

progArgs :: IORef [String]
progArgs = primPerformIO $ do
  args <- primGetArgs
  newIORef args

getArgs :: IO [String]
getArgs = readIORef progArgs

withArgs :: forall a . [String] -> IO a -> IO a
withArgs as ioa = do
  old <- readIORef progArgs
  writeIORef progArgs as
  a <- ioa
  writeIORef progArgs old
  return a

foreign import ccall "getenv" c_getenv :: CString -> IO CString

lookupEnv :: String -> IO (Maybe String)
lookupEnv var = do
  cptr <- withCAString var c_getenv
  if cptr == nullPtr then
    return Nothing
   else
    Just <$> peekCAString cptr

-- XXX implement this
getProgName :: IO String
getProgName = return "???"
