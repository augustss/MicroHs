-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Ident(
  Line, Col,
  Ident,
  mkIdent, unIdent, isIdent,
  qualIdent, showIdent,
  setSLocIdent, getSLocIdent,
  ppIdent,
  mkIdentSLoc,
  IdentModule,
  mkIdentModuleSLoc, mkIdentModule, unIdentModule, identModuleOf, ppIdentModule,
  getSLocIdentModule,
  isLower_, isIdentChar, isOperChar, isConIdent,
  dummyIdent, isDummyIdent,
  headIdent,
  unQualIdent,
  unQualString,
  qualOf,
  addIdentSuffix,
  SLoc(..), noSLoc,
  showSLoc,
  ) where
import Prelude hiding(head)
import Data.Char
import Text.PrettyPrint.HughesPJLite
import GHC.Stack
import MicroHs.List(dropEnd)

import Data.Text(Text, pack, unpack, append, head)
import Compat

{-
-- Uncomment this section, and comment out the two lines above
-- to use String instead of Text.
type Text = String
pack :: String -> Text
pack x = x
unpack :: Text -> String
unpack x = x
append :: Text -> Text -> Text
append = (++)
head :: Text -> Char
head (c:_) = c
head _ = undefined
appendDot :: Text -> Text -> Text
appendDot x y = x ++ ('.':y)
-}

type Line = Int
type Col  = Int

data SLoc = SLoc FilePath Line Col
--  deriving (Eq)

instance Show SLoc where
  show (SLoc f l c) = show f ++ "," ++ show l ++ ":" ++ show c

data Ident = Ident SLoc Text
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

getSLocIdent :: Ident -> SLoc
getSLocIdent (Ident l _) = l

newtype IdentModule = IM { unIM :: Ident }
  deriving (Eq, Ord)

instance Show IdentModule where
  show = show . unIM

identModuleOf :: Ident -> IdentModule
identModuleOf = IM

mkIdentModuleSLoc :: SLoc -> String -> IdentModule
mkIdentModuleSLoc l s = IM (mkIdentSLoc l s)

mkIdentModule :: String -> IdentModule
mkIdentModule s = IM (mkIdent s)

unIdentModule :: IdentModule -> String
unIdentModule = unIdent . unIM

getSLocIdentModule :: IdentModule -> SLoc
getSLocIdentModule = getSLocIdent . unIM

noSLoc :: SLoc
noSLoc = SLoc "" 0 0

mkIdent :: String -> Ident
mkIdent = mkIdentSLoc noSLoc

mkIdentSLoc :: SLoc -> String -> Ident
mkIdentSLoc l = Ident l . pack

unIdent :: Ident -> String
unIdent (Ident _ s) = unpack s

setSLocIdent :: SLoc -> Ident -> Ident
setSLocIdent l (Ident _ s) = Ident l s

showIdent :: Ident -> String
showIdent (Ident _ i) = unpack i

ppIdent :: Ident -> Doc
ppIdent (Ident _ i) = text $ unpack i

ppIdentModule :: IdentModule -> Doc
ppIdentModule = ppIdent . unIM

isIdent :: String -> Ident -> Bool
isIdent s (Ident _ i) = pack s == i

qualIdent :: HasCallStack =>
             IdentModule -> Ident -> Ident
qualIdent (IM (Ident _ qi)) (Ident loc i) = Ident loc (appendDot qi i)

addIdentSuffix :: Ident -> String -> Ident
addIdentSuffix (Ident loc i) s = Ident loc (i `append` pack s)

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

unQualIdent :: Ident -> Ident
unQualIdent (Ident l s) = Ident l (pack $ unQualString $ unpack s)

qualOf :: Ident -> IdentModule
qualOf (Ident loc s) = IM $ Ident loc (pack $ dropEnd (length (unQualString s') + 1) s')
  where s' = unpack s

headIdent :: Ident -> Char
headIdent (Ident _ i) = head i

isConIdent :: Ident -> Bool
isConIdent i@(Ident _ t) =
  let c = headIdent i
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

showSLoc :: SLoc -> String
showSLoc (SLoc fn l c) =
  if null fn then "no location" else
  show fn ++ ": " ++
    if l == 0  then "no location" else
    if l == -1 then "end-of-file" else
    "line " ++ show l ++ ", col " ++ show c
