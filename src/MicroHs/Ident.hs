-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Ident(
  Line, Col,
  Ident,
  IdentString,
  mkIdent, mkQIdent, unIdent, mkIdentS,
  qualIdent, showIdent, setIdentSLoc,
  qualOfIdent, baseOfIdent,
  ppIdent,
  mkIdentSLoc, mkQIdentSLoc,
  isLower_, isIdentChar, isOperChar, isConIdent,
  dummyIdent, isDummyIdent,
  slocIdent,
  headIdent,
  unQualIdent,
  qualOf,
  addIdentSuffix,
  SLoc(..), noSLoc,
  showSLoc,
  ) where
import Prelude hiding(head)
import Data.Char
import Data.Text(Text, pack, unpack, append, head)
import Text.PrettyPrint.HughesPJLite
import GHC.Stack
import Compat

type Line = Int
type Col  = Int

data SLoc = SLoc FilePath Line Col
--  deriving (Eq)

instance Show SLoc where
  show (SLoc f l c) = show f ++ "," ++ show l ++ ":" ++ show c

type IdentString = Text

data Ident = Ident SLoc IdentString | QIdent SLoc IdentString IdentString
  deriving (Show)

instance Eq Ident where
  Ident  _    i == Ident  _    j  =  i == j
  QIdent _ qi i == QIdent _ qj j  =  qi == qj && i == j
  _             == _              =  False

instance Ord Ident where
  compare (Ident _ i)     (Ident _ j)     = compare i j
  compare (Ident _ _)     (QIdent _ _ _)  = LT
  compare (QIdent _ _ _)  (Ident _ _)     = GT
  compare (QIdent _ qi i) (QIdent _ qj j) = compare qi qj Prelude.<> compare i j
{-
  Ident _ i <  Ident _ j  =  i <  j
  Ident _ i <= Ident _ j  =  i <= j
  Ident _ i >  Ident _ j  =  i >  j
  Ident _ i >= Ident _ j  =  i >= j
-}

--instance Show Ident where
--  show = showIdent

slocIdent :: Ident -> SLoc
slocIdent (Ident l _) = l
slocIdent (QIdent l _ _) = l

baseOfIdent :: Ident -> IdentString
baseOfIdent (Ident _ t) = t
baseOfIdent (QIdent _ _ t) = t

qualOfIdent :: Ident -> Maybe IdentString
qualOfIdent (Ident _ _) = Nothing
qualOfIdent (QIdent _ q _) = Just q

noSLoc :: SLoc
noSLoc = SLoc "" 0 0

mkIdentS :: IdentString -> Ident
mkIdentS s = Ident noSLoc s

mkIdent :: String -> Ident
mkIdent = mkIdentSLoc noSLoc

mkQIdent :: String -> String -> Ident
mkQIdent = mkQIdentSLoc noSLoc

mkIdentSLoc :: SLoc -> String -> Ident
mkIdentSLoc l s = Ident l (pack s)

mkQIdentSLoc :: SLoc -> String -> String -> Ident
mkQIdentSLoc l q s = QIdent l (pack q) (pack s)

unIdent :: Ident -> String
unIdent (Ident _ s) = unpack s
unIdent (QIdent _ q i) = unpack q ++ "." ++ unpack i

setIdentSLoc :: SLoc -> Ident -> Ident
setIdentSLoc l (Ident _ s) = Ident l s
setIdentSLoc l (QIdent _ q s) = QIdent l q s

showIdent :: Ident -> String
showIdent (Ident _ i) = unpack i
showIdent (QIdent _ q i) = unpack q ++ "." ++ unpack i

ppIdent :: Ident -> Doc
ppIdent i = text $ showIdent i

qualIdent :: HasCallStack =>
             Ident -> Ident -> Ident
qualIdent (Ident _ qi) (Ident loc i) = QIdent loc qi i
qualIdent (Ident _ qi) (QIdent loc qi' i) = QIdent loc (qi `appendDot` qi') i
qualIdent (QIdent _ _ _) _ = error "qualIdent"
  -- Ident loc (qi `append` "." `append` i)
  -- Ident loc (qi `appendDot` i)

addIdentSuffix :: Ident -> String -> Ident
addIdentSuffix (Ident loc i) s = Ident loc (i `append` pack s)
addIdentSuffix (QIdent loc qi i) s = QIdent loc qi (i `append` pack s)

{-
unQualString :: HasCallStack =>
                String -> String
unQualString [] = ""
unQualString s@(c:_) =
  if isUpper c then
    case dropWhile (/= '.') s of
      "" -> s
      '.':r -> unQualString r
      _ -> undefined -- This cannot happen, but GHC doesn't know that
  else
    s
-}
unQualIdent :: Ident -> Ident
unQualIdent (QIdent l _ s) = Ident l s
unQualIdent _ = error "unQualIdent"

qualOf :: Ident -> Ident
qualOf (QIdent loc qi _) = Ident loc qi
qualOf _ = error "qualOf"

headIdent :: Ident -> Char
headIdent (Ident _ i) = head i
headIdent (QIdent _ _ _) = error "headIdent"

isConIdent :: HasCallStack => Ident -> Bool
isConIdent i =
  let t = baseOfIdent i
      c = head t
  in  isUpper c || c == ':' || c == ',' || t == pack "[]"  || t == pack "()"

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
isDummyIdent (Ident _ s) = s == pack "_"
isDummyIdent _ = False

showSLoc :: SLoc -> String
showSLoc (SLoc fn l c) =
  if null fn then "no location" else
  show fn ++ ": " ++
    if l == 0  then "no location" else
    if l == -1 then "end-of-file" else
    "line " ++ show l ++ ", col " ++ show c
