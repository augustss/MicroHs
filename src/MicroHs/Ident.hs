-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Ident(
  Line, Col,
  Ident,
  mkIdent, unIdent, isIdent,
  qualIdent, showIdent, setSLocIdent,
  ppIdent,
  mkIdentSLoc,
  isLower_, isIdentChar, isOperChar, isConIdent,
  dummyIdent, isDummyIdent,
  slocIdent,
  headIdent,
  unQualIdent,
  unQualString,
  qualOf,
  addIdentSuffix,
  SLoc(..), noSLoc,
  showSLoc, slocFile,
  ) where
import Prelude(); import MHSPrelude hiding(head)
import Data.Char
import Text.PrettyPrint.HughesPJLite
import GHC.Stack
import MicroHs.List(dropEnd)
import MicroHs.MRnf

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

instance MRnf SLoc where
  mrnf (SLoc a b c) = mrnf a `seq` mrnf b `seq` mrnf c

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

instance MRnf Ident where
  mrnf (Ident a b) = mrnf a `seq` mrnf b

slocIdent :: Ident -> SLoc
slocIdent (Ident l _) = l

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

isIdent :: String -> Ident -> Bool
isIdent s (Ident _ i) = pack s == i

qualIdent :: HasCallStack =>
             Ident -> Ident -> Ident
qualIdent (Ident _ qi) (Ident loc i) =
  -- Ident loc (qi `append` "." `append` i)
  Ident loc (qi `appendDot` i)

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

qualOf :: Ident -> Ident
qualOf (Ident loc s) = Ident loc (pack $ dropEnd (length (unQualString s') + 1) s')
  where s' = unpack s

headIdent :: Ident -> Char
headIdent (Ident _ i) = head i

isConIdent :: Ident -> Bool
isConIdent i@(Ident _ t) =
  let c = headIdent i
  in  isUpper c || c == ':' || c == ',' || t == pack "[]"  || t == pack "()" || t == pack "->"

isOperChar :: Char -> Bool
isOperChar '@' = True
isOperChar '\\' = True
isOperChar '=' = True
isOperChar '+' = True
isOperChar '-' = True
isOperChar ':' = True
isOperChar '<' = True
isOperChar '>' = True
isOperChar '.' = True
isOperChar '!' = True
isOperChar '#' = True
isOperChar '$' = True
isOperChar '%' = True
isOperChar '^' = True
isOperChar '&' = True
isOperChar '*' = True
isOperChar '/' = True
isOperChar '|' = True
isOperChar '~' = True
isOperChar '?' = True
isOperChar '\x2237' = True  -- ::
isOperChar '\x21d2' = True  -- =>
isOperChar '\x2192' = True  -- ->
isOperChar '\x2190' = True  -- <-
isOperChar '\x2200' = True  -- forall
isOperChar _ = False

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

slocFile :: SLoc -> FilePath
slocFile (SLoc f _ _) = f
