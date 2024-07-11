-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Ident(
  Line, Col,
  Ident(..),
  mkIdent, unIdent, isIdent,
  qualIdent, showIdent, setSLocIdent,
  ppIdent,
  mkIdentSLoc,
  isLower_, isIdentChar, isOperChar, isConIdent,
  dummyIdent, isDummyIdent,
  unQualIdent,
  unQualString,
  addIdentSuffix,
  SLoc(..), noSLoc,
  showSLoc,
  ) where
import Data.Eq
import Prelude
import Data.Char
import Text.PrettyPrint.HughesPJLite
import GHC.Stack

type Line = Int
type Col  = Int

data SLoc = SLoc !FilePath !Line !Col
--  deriving (Eq)

instance Show SLoc where
  show (SLoc f l c) = show f ++ "," ++ show l ++ ":" ++ show c

data Ident = Ident !SLoc String
  --deriving (Show)

instance Eq Ident where
  Ident _ i == Ident _ j  =  i == j

instance Ord Ident where
  compare (Ident _ i) (Ident _ j) = compare i j
  Ident _ i <  Ident _ j  =  i <  j
  Ident _ i <= Ident _ j  =  i <= j
  Ident _ i >  Ident _ j  =  i >  j
  Ident _ i >= Ident _ j  =  i >= j

instance Show Ident where
  show = showIdent

noSLoc :: SLoc
noSLoc = SLoc "" 0 0

mkIdent :: String -> Ident
mkIdent = Ident noSLoc

mkIdentSLoc :: SLoc -> String -> Ident
mkIdentSLoc = Ident

unIdent :: Ident -> String
unIdent (Ident _ s) = s

setSLocIdent :: SLoc -> Ident -> Ident
setSLocIdent l (Ident _ s) = Ident l s

showIdent :: Ident -> String
showIdent (Ident _ i) = i

ppIdent :: Ident -> Doc
ppIdent (Ident _ i) = text i

isIdent :: String -> Ident -> Bool
isIdent s (Ident _ i) = s == i

qualIdent :: HasCallStack =>
             Ident -> Ident -> Ident
qualIdent (Ident _ qi) (Ident loc i) = Ident loc (qi ++ "." ++ i)

addIdentSuffix :: Ident -> String -> Ident
addIdentSuffix (Ident loc i) s = Ident loc (i ++ s)

unQualString :: HasCallStack =>
                String -> String
unQualString str = let (n, res) = unqual str 0
                   in dropFromEnd n res
  where
    -- | Drop n closing parentheses from the end of a string
    dropFromEnd :: Int -> String -> String
    dropFromEnd k [] | k > 0 =
      error $ "unmatched parenthesis in Ident, expecting " ++ show k ++ " more closing parentheses"
    dropFromEnd 0 str = str
    dropFromEnd k str =
      let (x,xs) = last' str []
      in if x == ')'
        then dropFromEnd (k-1) xs
        else error $ "expected closing parentheses at end of ident, instead found " ++ show x
      where
        -- | Removes the last element of a list, but also returns it
        last' :: [a] -> [a] -> (a, [a])
        last' [] _ = error "empty string"
        last' [x] xs = (x, reverse xs)
        last' (x:xs) ys = last' xs (x : ys)

-- | Unqualified a string, keeping track of how many opening parentheses are opened
unqual :: HasCallStack => String -> Int -> (Int, String)
unqual [] _ = (0, "")
unqual s@(c:_) n =
  if isIdentChar c then
    case dropWhileCount (/= '.') '(' 0 s of
      (_, "") -> (n, s) -- string is not qualified, do not modify it
      (m, '.':r) -> unqual r (n + m)
      _ -> undefined -- This cannot happen, but GHC doesn't know that
  else
    (0, s)
  where
    -- | Like dropWhile, but also counts how many times it sees a specific character
    -- example, dropping characters until a dot, while counting how many open aprentheses are observed:
    --
    -- > dropWhileCount ((/=) '.') '(' 0 "(123.45)"
    -- (1,".45)")
    dropWhileCount :: (Char -> Bool) -> Char -> Int -> String -> (Int, String)
    dropWhileCount _ _ n [] = (n, [])
    dropWhileCount test toCount n (x:xs) =
      if test x
        then let (n', res) = dropWhileCount test toCount n xs
                 n'' = if x == toCount then n' + 1 else n'
             in (n'', res)
        else (n, (x:xs))

unQualIdent :: Ident -> Ident
unQualIdent (Ident l s) = Ident l (unQualString s)

isConIdent :: Ident -> Bool
isConIdent (Ident _ i) =
  let
    c = head i
  in isUpper c || c == ':' || c == ',' || i == "[]"  || i == "()"

isOperChar :: Char -> Bool
isOperChar c = elem c operChars
  where operChars :: String
        operChars = "@\\=+-:<>.!#$%^&*/|~?\x2237\x21d2\x2192\x2190\x2200"
--                                           ::    =>    ->    <- forall

isIdentChar :: Char -> Bool
isIdentChar c = isLower_ c || isUpper c || isDigit c || c == '\''

isLower_ :: Char -> Bool
isLower_ c = isLower c || c == '_'

dummyIdent :: Ident
dummyIdent = mkIdent "_"

isDummyIdent :: Ident -> Bool
isDummyIdent (Ident _ "_") = True
isDummyIdent _ = False

showSLoc :: SLoc -> String
showSLoc (SLoc fn l c) =
  if null fn then "no location" else
  show fn ++ ": " ++
    if l == 0  then "no location" else
    if l == -1 then "end-of-file" else
    "line " ++ show l ++ ", col " ++ show c
