module Data.String(
  IsString(..),
  String,
  lines, unlines,
  words, unwords,
  ) where

import qualified Data.ByteString.Char8 as BS8

class IsString a where
  fromString :: String -> a

instance IsString [Char] where
  fromString s = s

instance IsString BS8.ByteString where
  fromString = BS8.pack
