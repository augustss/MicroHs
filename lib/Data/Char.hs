module Data.Char(module Data.Char) where
import qualified Primitives
import Data.Bool
import Data.Int

type Char = Primitives.Char
type String = [Char]

chr :: Int -> Char
chr = Primitives.primChr

ord :: Char -> Int
ord = Primitives.primOrd

isLower :: Char -> Bool
isLower c = (ord 'a' <= ord c) && (ord c <= ord 'z')

isUpper :: Char -> Bool
isUpper c = (ord 'A' <= ord c) && (ord c <= ord 'Z')

isLetter :: Char -> Bool
isLetter c = isLower c || isUpper c

isDigit :: Char -> Bool
isDigit c = (ord '0' <= ord c) && (ord c <= ord '9')

eqChar :: Char -> Char -> Bool
eqChar a b = ord a == ord b

neChar :: Char -> Char -> Bool
neChar a b = not (eqChar a b)
