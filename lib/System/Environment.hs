-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.Environment(
  getArgs,
  getProgName,
  withArgs,
  lookupEnv,
  getEnv,
  getEnvironment,
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives
import Data.Bifunctor(second)
import Data.List(span)
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO

-- primArgRef returns an array containing a list of strings.
-- The first element is the program name, the rest are the arguments.
getArgs :: IO [String]
getArgs = do
  aa <- primGetArgRef
  as <- primArrRead aa 0
  return $ tail as                          -- drop program name

getProgName :: IO String
getProgName = do
  aa <- primGetArgRef
  as <- primArrRead aa 0
  return $ head as                          -- get program name

withArgs :: forall a . [String] -> IO a -> IO a
withArgs as ioa = do
  aa <- primGetArgRef
  old <- primArrRead aa 0
  primArrWrite aa 0 $ head old : as         -- keep program name
  a <- ioa
  primArrWrite aa 0 old
  return a

foreign import ccall "getenv" c_getenv :: CString -> IO CString

lookupEnv :: String -> IO (Maybe String)
lookupEnv var = do
  cptr <- withCAString var c_getenv
  if cptr == nullPtr then
    return Nothing
   else
    Just <$> peekCAString cptr

getEnv :: String -> IO String
getEnv var = do
  mval <- lookupEnv var
  case mval of
    Nothing  -> error $ "getEnv: not found " ++ var
    Just val -> return val

foreign import capi "unistd.h value environ" c_environ :: Ptr CString

getEnvironment :: IO [(String, String)]
getEnvironment = do
  cstrs <- peekArray0 nullPtr c_environ
  strs <- mapM peekCAString cstrs
  let splitEq = second (drop 1) . span (/= '=')
  return $ map splitEq strs
