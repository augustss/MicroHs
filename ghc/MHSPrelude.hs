{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
module MHSPrelude(
  module Prelude,
  module Control.Arrow,
  module Control.DeepSeq,
  module Control.Exception,
  module Data.Version,
  module Text.Read,
  module MHSPrelude,
  Type,
  Int64,
  HasCallStack
  ) where
import Prelude
import Control.Arrow(first, second)
import Control.DeepSeq
import Control.Exception
import Data.Int
import Data.Maybe
import Data.List
import Data.Text(Text, append, pack)
import Data.Version
import GHC.Stack
import Data.Kind
import System.Environment
import System.IO
import Text.Read(readMaybe)

#include "MachDeps.h"

------- Text --------

-- base-4.19.2.0 seems to lack this???
#if 0
instance NFData Text where
  rnf t = seq t ()
#endif

------- List --------

takeWhileEnd :: forall a . (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

stripSuffix :: forall a . Eq a => [a] -> [a] -> Maybe [a]
stripSuffix s t =
  case stripPrefix (reverse s) (reverse t) of
    Nothing -> Nothing
    Just x -> Just (reverse x)

------- IO --------

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM path m = do
  r <- (try $ openFile path m) :: IO (Either IOError Handle)
  case r of
    Left _ -> return Nothing
    Right h -> return (Just h)

openBinaryFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openBinaryFileM path m = do
  r <- (try $ openBinaryFile path m) :: IO (Either IOError Handle)
  case r of
    Left _ -> return Nothing
    Right h -> return (Just h)

openTmpFile :: String -> IO (String, Handle)
openTmpFile tmplt = do
  mtmp <- lookupEnv "TMPDIR"
  let tmp = fromMaybe "/tmp" mtmp
  res <- try $ openTempFile tmp tmplt
  case res of
    Right x -> return x
    Left (_::SomeException) -> openTempFile "." tmplt

------- Read --------

_usingMhs :: Bool
_usingMhs = False

_wordSize :: Int
_wordSize = WORD_SIZE_IN_BITS

_isWindows :: Bool
_isWindows = False

appendDot :: Text -> Text -> Text
appendDot x y = x `append` pack "." `append` y

wantGMP :: Bool
wantGMP = False

wantImath :: Bool
wantImath = False

compiledWithMhs :: Bool
compiledWithMhs = False

---
-- This belongs in MicroHs.Error, but Hugs cannot derive Tyoeable
-- and GHC doesn't allow explicit Typeable instances.
data MhsError
  = MhsSyntaxError SLoc String String
  | MhsTypeError   SLoc String String String  -- header, expected, actual
  | MhsUndefError  SLoc String String         -- name space, ident
  | MhsAmbigError  SLoc String String String  -- name space, ident, alts
  | MhsOtherError  SLoc String
  | MhsError            String                -- no location
data SLoc = SLoc FilePath Int Int
  deriving (Show, Eq)
