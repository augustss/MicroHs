module Data.Version(
  Version(..),
  showVersion,
  makeVersion
  ) where
import Prelude(); import MiniPrelude
import Data.List(intercalate)

data Version = Version { versionBranch :: [Int] }
  deriving (Show, Eq, Ord)

showVersion :: Version -> String
showVersion (Version b) = intercalate "." (map show b)

{-
parseVersion :: ReadP Version
parseVersion = do branch <- sepBy1 (fmap read (munch1 isDigit)) (char '.')
                  pure Version{versionBranch=branch}
-}
makeVersion :: [Int] -> Version
makeVersion b = Version b
