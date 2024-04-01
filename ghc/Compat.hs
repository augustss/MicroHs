-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
-- Functions for GHC that are defined in the UHS libs.
module Compat(module Compat, Type) where
import Data.Char
import Data.Maybe
--import qualified Control.Monad as M
import Control.Exception
import GHC.Records
import GHC.Types
import System.Environment
import System.IO

------- IO --------

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM path m = do
  r <- (try $ openFile path m) :: IO (Either IOError Handle)
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

-- Convert string in scientific notation to a rational number.
readRational :: String -> Rational
readRational "" = undefined
readRational acs@(sgn:as) | sgn == '-' = negate $ rat1 as
                          | otherwise  =          rat1 acs
  where
    rat1 s1 =
      case span isDigit s1 of
        (ds1, cr1) | ('.':r1) <- cr1                   -> rat2 f1 r1
                   | (c:r1)   <- cr1, toLower c == 'e' -> rat3 f1 r1
                   | otherwise                         -> f1
          where f1 = toRational (read ds1 :: Integer)

    rat2 f1 s2 =
      case span isDigit s2 of
        (ds2, cr2) | (c:r2) <- cr2, toLower c == 'e' -> rat3 f2 r2
                   | otherwise                       -> f2
          where f2 = f1 + toRational (read ds2 :: Integer) * 10 ^^ (negate $ length ds2)

    rat3 f2 ('+':s) = f2 * expo s
    rat3 f2 ('-':s) = f2 / expo s
    rat3 f2      s  = f2 * expo s

    expo s = 10 ^ (read s :: Int)

usingMhs :: Bool
usingMhs = False

_wordSize :: Int
_wordSize = 64

_isWindows :: Bool
_isWindows = False

-- This cannot be implemented with GHC.
rnfNoErr :: forall a . a -> ()
rnfNoErr _ = ()

-----------------------------------
-- Virtual fields for tuples.

instance forall a b . HasField "_1" (a, b) a where
  getField (a, _) = a
instance forall a b . HasField "_2" (a, b) b where
  getField (_, b) = b

instance forall a b c . HasField "_1" (a, b, c) a where
  getField (a, _, _) = a
instance forall a b c . HasField "_2" (a, b, c) b where
  getField (_, b, _) = b
instance forall a b c . HasField "_3" (a, b, c) c where
  getField (_, _, c) = c

instance forall a b c d . HasField "_1" (a, b, c, d) a where
  getField (a, _, _, _) = a
instance forall a b c d . HasField "_2" (a, b, c, d) b where
  getField (_, b, _, _) = b
instance forall a b c d . HasField "_3" (a, b, c, d) c where
  getField (_, _, c, _) = c
instance forall a b c d . HasField "_4" (a, b, c, d) d where
  getField (_, _, _, d) = d
