-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MHSPrelude(
  module Prelude,
  module MHSPrelude,
--  module Control.Monad.Fail,
  module Control.DeepSeq
  module Data.Monoid,
  module Data.Semigroup,
  (<$>), Applicative(..), (*>),
  ) where
import Hugs.Prelude()
import Prelude hiding(fail)
import qualified Prelude
import Control.Applicative
import Control.DeepSeq
import Control.Exception(Exception, try)
--import Control.Monad.Fail
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Text(Text, append, pack)
import Data.Version
import Debug.Trace
import System.IO
import System.Environment

------- List --------

intercalate :: forall a . [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

stripPrefix :: forall a . Eq a => [a] -> [a] -> Maybe [a]
stripPrefix = stripPrefixBy (==)

stripPrefixBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
stripPrefixBy eq [] s = Just s
stripPrefixBy eq (c:cs) [] = Nothing
stripPrefixBy eq (c:cs) (d:ds) | eq c d = stripPrefixBy eq cs ds
                               | otherwise = Nothing

takeWhileEnd :: forall a . (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && Prelude.null xs then [] else x : xs) []

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)

breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p = spanEnd (not . p)

------- Version --------

makeVersion :: [Int] -> Version
makeVersion b = Version b []

------- Debug --------

traceM :: Monad m => String -> m ()
traceM s = do () <- trace s $ return (); return ()

-------

void :: Functor f => f a -> f ()
void = fmap (const ())

asum :: Alternative f => [f a] -> f a
asum [] = empty
asum (a:as) = a <|> asum as


------- List --------

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

wantGMP :: Bool
wantGMP = False

compiledWithMhs :: Bool
compiledWithMhs = False
