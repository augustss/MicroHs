module Data.Version(
  Version(..),
  showVersion,
  makeVersion,
  parseVersion,
  ) where
import qualified Prelude(); import MiniPrelude
import Control.DeepSeq.Class
import Data.List(intercalate)
import Text.ParserCombinators.ReadP
import Text.Read

data Version = Version { versionBranch :: [Int], versionTags :: [String] }
  deriving (Show, Eq, Ord)

instance NFData Version where
  rnf (Version x y) = rnf x `seq` rnf y

showVersion :: Version -> String
showVersion (Version b _) = intercalate "." (map show b)

parseVersion :: ReadP Version
parseVersion = do branch <- sepBy1 (fmap read (munch1 isDigit)) (char '.')
                  pure Version{versionBranch=branch, versionTags=[]}

makeVersion :: [Int] -> Version
makeVersion b = Version b []
