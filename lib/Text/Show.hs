module Text.Show(module Text.Show) where
import Primitives
import Data.Bool_Type
import Data.Char_Type
import Data.List_Type

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show      :: a -> String
  showList  :: [a] -> ShowS

  showsPrec _ x s   = show x ++ s
  show x            = showsPrec 0 x ""
  showList []     s = '[' : ']' : s
  showList (x:xs) s = '[' : shows x (shl xs)
    where
      shl []     = ']' : s
      shl (y:ys) = ',' : shows y (shl ys)

shows :: forall a . Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen False sh = sh
showParen True  sh = \ x -> '(' : sh (')' : x)
