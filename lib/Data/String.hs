module Data.String(
  IsString(..),
  String,
  lines, unlines,
  words, unwords,
  ) where
import qualified Prelude()
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.List
import {-# SOURCE #-} Data.Typeable

class IsString a where
  fromString :: String -> a

instance (a ~ Char) => IsString [a] where
  fromString s = s
