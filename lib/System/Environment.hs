-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.Environment(module System.Environment) where
import Prelude
import Primitives
import Foreign.C.String
import Foreign.Ptr

getArgs :: IO [String]
getArgs = primGetArgs

withDropArgs :: forall a . Int -> IO a -> IO a
withDropArgs = primWithDropArgs

foreign import ccall "getenv" getenvc :: CString -> IO CString

lookupEnv :: String -> IO (Maybe String)
lookupEnv var = do
  cptr <- withCAString var getenvc
  if cptr == nullPtr then
    return Nothing
   else
    Just <$> peekCAString cptr
