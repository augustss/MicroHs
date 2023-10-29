module Text.Show(module Text.Show) where
import Data.Char
import Data.Int

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

--showChar :: Char -> ShowS
--showParen :: Bool -> ShowS -> ShowS
--showString :: String -> ShowS
--shows :: forall a . Show a => a -> ShowS
