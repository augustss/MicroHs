-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
-- Functions for Hugs that are defined in the UHS libs.
module Compat(module Compat) where
import Prelude(); import MHSPrelude
import Control.Exception
import Data.Maybe
import Data.List
import Data.Text(Text, append, pack)
import System.Environment
import System.IO
import Debug.Trace

------- List --------

intercalate :: forall a . [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

stripSuffix :: forall a . Eq a => [a] -> [a] -> Maybe [a]
stripSuffix s t =
  case stripPrefix (reverse s) (reverse t) of
    Nothing -> Nothing
    Just x -> Just (reverse x)

------- IO --------

type SomeException = Exception

data TextEncoding = UTF8

displayException :: Exception -> String
displayException = show

utf8 :: TextEncoding
utf8 = UTF8

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding "UTF-8//ROUNDTRIP" = return UTF8
mkTextEncoding _ = error "unknown text encoding"

-- Always in UTF8 mode
hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding _ _ = return ()

lookupEnv :: String -> IO (Maybe String)
lookupEnv var = do
  r <- try $ getEnv var
  case r of
    Left _ -> return Nothing
    Right s -> return (Just s)

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM path m = do
  r <- (try $ openFile path m)
  case r of
    Left _ -> return Nothing
    Right h -> return (Just h)

splitTmp :: String -> (String, String)
splitTmp tmpl = 
  case span (/= '.') (reverse tmpl) of
    (rsuf, "") -> (tmpl, "")
    (rsuf, _:rpre) -> (reverse rpre, '.':reverse rsuf)

-- Sloppy implementation of openTempFile
openTempFile' :: (FilePath -> IOMode -> IO Handle) -> FilePath -> String -> IO (String, Handle)
openTempFile' open tmp tmplt = do
  let (pre, suf) = splitTmp tmplt
      loop n = do
        let fn = tmp ++ "/" ++ pre ++ show n ++ suf
        mh <- openFileM fn ReadMode
        case mh of
          Just h -> do
            hClose h
            loop (n+1 :: Int)
          Nothing -> do
            h <- open fn ReadWriteMode
            return (fn, h)
  loop 0

openTempFile :: FilePath -> String -> IO (String, Handle)
openTempFile = openTempFile' openFile

openTmpFile :: String -> IO (String, Handle)
openTmpFile tmplt = do
  mtmp <- lookupEnv "TMPDIR"
  let tmp = fromMaybe "/tmp" mtmp
  res <- try $ openTempFile tmp tmplt
  case res of
    Right x -> return x
    Left (_::SomeException) -> openTempFile "." tmplt

------- Debug --------

traceM :: forall m . Monad m => String -> m ()
traceM s = trace s (return ())

------- Read --------

usingMhs :: Bool
usingMhs = False

_wordSize :: Int
_wordSize = 64

_isWindows :: Bool
_isWindows = False

-- This cannot be implemented with Hugs.
rnfNoErr :: forall a . a -> ()
rnfNoErr _ = ()

-- This cannot be implemented with Hugs.
rnfErr :: forall a . a -> ()
rnfErr _ = ()

appendDot :: Text -> Text -> Text
appendDot x y = x `append` pack "." `append` y
