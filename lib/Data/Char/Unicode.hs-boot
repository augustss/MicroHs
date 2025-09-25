module Data.Char.Unicode where
import qualified Prelude()
import Data.Bool_Type
import Data.Bounded
import Data.Char_Type
import Data.Enum_Class
import Data.Eq
import Data.Ord
import Text.Show

data GeneralCategory
  = UppercaseLetter       -- Lu: Letter, Uppercase
  | LowercaseLetter       -- Ll: Letter, Lowercase
  | TitlecaseLetter       -- Lt: Letter, Titlecase
  | ModifierLetter        -- Lm: Letter, Modifier
  | OtherLetter           -- Lo: Letter, Other
  | NonSpacingMark        -- Mn: Mark, Non-Spacing
  | SpacingCombiningMark  -- Mc: Mark, Spacing Combining
  | EnclosingMark         -- Me: Mark, Enclosing
  | DecimalNumber         -- Nd: Number, Decimal
  | LetterNumber          -- Nl: Number, Letter
  | OtherNumber           -- No: Number, Other
  | ConnectorPunctuation  -- Pc: Punctuation, Connector
  | DashPunctuation       -- Pd: Punctuation, Dash
  | OpenPunctuation       -- Ps: Punctuation, Open
  | ClosePunctuation      -- Pe: Punctuation, Close
  | InitialQuote          -- Pi: Punctuation, Initial quote
  | FinalQuote            -- Pf: Punctuation, Final quote
  | OtherPunctuation      -- Po: Punctuation, Other
  | MathSymbol            -- Sm: Symbol, Math
  | CurrencySymbol        -- Sc: Symbol, Currency
  | ModifierSymbol        -- Sk: Symbol, Modifier
  | OtherSymbol           -- So: Symbol, Other
  | Space                 -- Zs: Separator, Space
  | LineSeparator         -- Zl: Separator, Line
  | ParagraphSeparator    -- Zp: Separator, Paragraph
  | Control               -- Cc: Other, Control
  | Format                -- Cf: Other, Format
  | Surrogate             -- Cs: Other, Surrogate
  | PrivateUse            -- Co: Other, Private Use
  | NotAssigned           -- Cn: Other, Not Assigned
  --deriving (Show, Eq, Ord, Enum, Bounded)
instance Show    GeneralCategory
instance Eq      GeneralCategory
instance Ord     GeneralCategory
instance Enum    GeneralCategory
instance Bounded GeneralCategory

isControl :: Char -> Bool
isPrint :: Char -> Bool
isSpace :: Char -> Bool
isUpper :: Char -> Bool
isLower :: Char -> Bool
isAlpha :: Char -> Bool
isAlphaNum :: Char -> Bool
isNumber :: Char -> Bool
isMark :: Char -> Bool
isSeparator :: Char -> Bool
isPunctuation :: Char -> Bool
isSymbol :: Char -> Bool
toTitle :: Char -> Char
toUpper :: Char -> Char
toLower :: Char -> Char
generalCategory :: Char -> GeneralCategory
