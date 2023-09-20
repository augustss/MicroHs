-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Ident(
  Line, Col, Loc,
  Ident(..),
  mkIdent, mkIdentLoc, unIdent, eqIdent, leIdent, qualIdent, showIdent, getSLocIdent, setSLocIdent,
  mkIdentSLoc,
  forceIdent,
  isLower_, isIdentChar, isOperChar, isConIdent,
  unQualString,
  SLoc(..), noSLoc, isNoSLoc,
  showSLoc
  ) where
import Prelude --Xhiding(showString)
import Data.Char
--Ximport Compat

type Line = Int
type Col  = Int
type Loc  = (Line, Col)

data SLoc = SLoc FilePath Line Col
  --Xderiving (Show, Eq)

data Ident = Ident SLoc String
  --Xderiving (Show, Eq)

noSLoc :: SLoc
noSLoc = SLoc "" 0 0

isNoSLoc :: SLoc -> Bool
isNoSLoc (SLoc "" 0 0) = True
isNoSLoc _ = False

mkIdent :: String -> Ident
mkIdent = Ident noSLoc

mkIdentSLoc :: SLoc -> String -> Ident
mkIdentSLoc = Ident

mkIdentLoc :: FilePath -> Loc -> String -> Ident
mkIdentLoc fn (l, c) s = Ident (SLoc fn l c) s

unIdent :: Ident -> String
unIdent (Ident _ s) = s

getSLocIdent :: Ident -> SLoc
getSLocIdent (Ident loc _) = loc

setSLocIdent :: SLoc -> Ident -> Ident
setSLocIdent l (Ident _ s) = Ident l s

showIdent :: Ident -> String
showIdent (Ident _ i) = i

eqIdent :: Ident -> Ident -> Bool
eqIdent (Ident _ i) (Ident _ j) = eqString i j

leIdent :: Ident -> Ident -> Bool
leIdent (Ident _ i) (Ident _ j) = leString i j

qualIdent :: Ident -> Ident -> Ident
qualIdent (Ident loc qi) (Ident _ i) = Ident loc (qi ++ "." ++ i)

unQualString :: String -> String
unQualString s =
  case span isIdentChar s of
    ("", r) -> r
    (r, "") -> r                       -- XXX bug!  swapping with next line goes wrong
    (_, '.':r) -> unQualString r
    x -> error $ "unQualString: " ++ showPair showString (showPair showString showString) (s, x)

isConIdent :: Ident -> Bool
isConIdent (Ident _ i) =
  let
    c = head i
  in isUpper c || eqChar c ':' || eqChar c ',' || eqString i "[]"  || eqString i "()"

isOperChar :: Char -> Bool
isOperChar c = elemBy eqChar c "@\\=+-:<>.!#$%^&*/|~?"

isIdentChar :: Char -> Bool
isIdentChar c = isLower_ c || isUpper c || isDigit c || eqChar c '\''

isLower_ :: Char -> Bool
isLower_ c = isLower c || eqChar c '_'

showSLoc :: SLoc -> String
showSLoc (SLoc fn l c) =
  if null fn then "no location" else
  showString fn ++ ": " ++ "line " ++ showInt l ++ ", col " ++ showInt c

-- Does not force location
forceIdent :: Ident -> ()
forceIdent (Ident _ s) = forceString s
