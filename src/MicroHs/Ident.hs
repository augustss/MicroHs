-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Ident(
  Line, Col, Loc,
  Ident(..),
  mkIdent, mkIdentLoc, unIdent, isIdent,
  qualIdent, showIdent, setSLocIdent,
  ppIdent,
  mkIdentSLoc,
  isLower_, isIdentChar, isOperChar, isConIdent,
  dummyIdent, isDummyIdent,
  unQualIdent,
  unQualString,
  addIdentSuffix,
  SLoc(..), noSLoc, isNoSLoc,
  showSLoc,
  ) where
import Data.Eq
import Prelude
import Data.Char
import Text.PrettyPrint.HughesPJ
import GHC.Stack

type Line = Int
type Col  = Int
type Loc  = (Line, Col)

data SLoc = SLoc !FilePath !Line !Col
  deriving (Eq)

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

isNoSLoc :: SLoc -> Bool
isNoSLoc (SLoc _ 0 0) = True
isNoSLoc _ = False

mkIdent :: String -> Ident
mkIdent = Ident noSLoc

mkIdentSLoc :: SLoc -> String -> Ident
mkIdentSLoc = Ident

mkIdentLoc :: FilePath -> Loc -> String -> Ident
mkIdentLoc fn (l, c) s = Ident (SLoc fn l c) s

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
unQualString [] = ""
unQualString s@(c:_) =
  if isIdentChar c then
    case dropWhile (/= '.') s of
      "" -> s
      '.':r -> unQualString r
      _ -> undefined -- This cannot happen, but GHC doesn't know that
  else
    s

unQualIdent :: Ident -> Ident
unQualIdent (Ident l s) = Ident l (unQualString s)

isConIdent :: Ident -> Bool
isConIdent (Ident _ i) =
  let
    c = head i
  in isUpper c || c == ':' || c == ',' || i == "[]"  || i == "()"

isOperChar :: Char -> Bool
isOperChar c = elem c "@\\=+-:<>.!#$%^&*/|~?"

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
  show fn ++ 
    if l == 0 && c == 0 then ""
    else ": line " ++ show l ++ ", col " ++ show c
