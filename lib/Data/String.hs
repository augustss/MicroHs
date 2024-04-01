module Data.String(
  IsString(..),
  String,
  lines, unlines,
  words, unwords,
  ) where
import Prelude()
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.List

class IsString a where
  fromString :: String -> a

instance IsString String where
  fromString s = s
