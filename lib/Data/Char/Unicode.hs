module Data.Char.Unicode (
        GeneralCategory (..), generalCategory,
        isControl,
        isPrint, isSpace, isUpper,
        isLower, isAlpha, isDigit,
        isAlphaNum,
        isPunctuation, isSymbol,
        toUpper, toLower
    ) where
import qualified Prelude(); import MiniPrelude
import Primitives(primOrd)
import Data.Bounded
import qualified Data.ByteString.Internal as BS
import System.Compress

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
  deriving (Show, Eq, Ord, Enum, Bounded)

isControl :: Char -> Bool
isControl c = bomb "isControl" c $
  case generalCategory c of
    Control -> True
    _       -> False

isPrint :: Char -> Bool
isPrint c = bomb "isPrint" c $
  case generalCategory c of
    LineSeparator      -> False
    ParagraphSeparator -> False
    Control            -> False
    Format             -> False
    Surrogate          -> False
    PrivateUse         -> False
    NotAssigned        -> False
    _                  -> True

isSpace :: Char -> Bool
isSpace c =  bomb "isSpace" c $
  generalCategory c == Space

isUpper :: Char -> Bool
isUpper c = bomb "isUpper" c $
  case generalCategory c of
    UppercaseLetter -> True
    TitlecaseLetter -> True
    _               -> False

isLower :: Char -> Bool
isLower c = bomb "isLower" c $
  case generalCategory c of
    LowercaseLetter -> True
    _               -> False

isAlpha :: Char -> Bool
isAlpha c = bomb "isAlpha" c $
  case generalCategory c of
    UppercaseLetter -> True
    LowercaseLetter -> True
    TitlecaseLetter -> True
    ModifierLetter  -> True
    OtherLetter     -> True
    _               -> False

isAlphaNum :: Char -> Bool
isAlphaNum c = bomb "isAlphaNum" c $
  case generalCategory c of
    UppercaseLetter -> True
    LowercaseLetter -> True
    TitlecaseLetter -> True
    ModifierLetter  -> True
    OtherLetter     -> True
    DecimalNumber   -> True
    LetterNumber    -> True
    OtherNumber     -> True
    _               -> False


isPunctuation :: Char -> Bool
isPunctuation c = bomb "isPunctuation" c $
  case generalCategory c of
    ConnectorPunctuation    -> True
    DashPunctuation         -> True
    OpenPunctuation         -> True
    ClosePunctuation        -> True
    InitialQuote            -> True
    FinalQuote              -> True
    OtherPunctuation        -> True
    _                       -> False

isSymbol :: Char -> Bool
isSymbol c = bomb "isSymbol" c $
  case generalCategory c of
    MathSymbol              -> True
    CurrencySymbol          -> True
    ModifierSymbol          -> True
    OtherSymbol             -> True
    _                       -> False

toUpper :: Char -> Char
toUpper c = bomb "toUpper" c $ convLU ucTable c

toLower :: Char -> Char
toLower c = bomb "toLower" c $ convLU lcTable c

-- Used to debug unintentional use of Unicode module
bomb :: String -> Char -> a -> a
--bomb s c _ = error $ "bomb " ++ s ++ show c
bomb _ _ a = a

-- XXX We could build a seatch tree and use binary search.
convLU :: [(Int, Int, Int)] -> Char -> Char
convLU t c = conv t
  where i = primOrd c
        conv [] = c
        conv ((l, h, d):lhds) | l <= i && i <= h = chr (i + d)
                              | otherwise = conv lhds

generalCategory :: Char -> GeneralCategory
generalCategory c =
  let i = primOrd c
  in  if i < 0 || i >= BS.length bytestringGCTable then
        NotAssigned
      else
        toEnum (fromEnum (BS.primBSindex bytestringGCTable i))

bytestringGCTable :: BS.ByteString
bytestringGCTable = BS.pack $ map (toEnum . fromEnum) charGCTable

charGCTable :: [Char]
charGCTable = decompressRLE $ decompress compressedGCTable

-- These table are generated by unicode/UniParse.hs
-- This is for Unicode 15.1.0
compressedGCTable :: [Char]
compressedGCTable =
  "LZ1\133\16\0\0\31\159\f\t\130\25\27\130\25\21\22\25\26\25\20\25\25\137\6\25\25\130\26\25\25\153\0\21\25\22\28\19\28\31\153\1\21\26\22\26\160\f\t\25\131\27\29\25\28\29\18\23\26\r\29\28\29\26\b\b\28\1\25\25\28\b\15\18\24\130\b\25\150\0\26\134\0\151\1\26\135\1\0\224\1.\224\16\25\224\1\22`\4\0\130\160\a\6\0\1\130\0\1\1\131\160\t\128\21\224'\2\224\"\0`\a\t\1\18\0\130\1\131\18\0\2\1\160\2\224\1\6\224\16\b@\18@&\128@\224\1-\0\134\160\147\1\1\0`\166\128\1\23\196\1\18\154\1\145\17\131\28\139\17\141\28\132\17\134\28\17\28\17\144\28\239\3@\28\30\17\28\0\1\16\16\17\130\1\25\0\131\16\28\28\0\25\130\0\16\0\16\0\0\1\144\0\16\136\0\162@\148`\233\224\1\r\4\132\1\0\1\26\128p\2\178\0\175\224%\15\224\1\2\4\29\132\3\5\5\224&\25\224\1\f\224\14\5\224%\15\224\1@\23\16\165\0\16\16\17\133\25\168\1\25\20\16\16\29\29\27\16\172\3\20\3\25\3\128\2\r\135\16\154\18\131\16\131\18\25\25\138\16\133\rB\217\31\27\25\25\29\29\138\3\25\r\130\25\159\18\17\137\18\148\3\137\6\131\25\18\18\3\226\18\25\18\134\3\r\31\29\133\3\17\17\3\3\29\131\3\18\18\137\6\130\18\29\29\18\141\25\16\r\18\3\157\18\154\3\16\16\216\31\18\138\3\18\141\16\137\6\160\18\136\3\17\17\29\130\25\17\16\16\3\27\27\149\18\131\3\17\136\3\17\130\31\3\17\132\3\16\16\142\25\16\152\18\130\3\16\16\25\16\138\18\132\16\151\18\28\133\18\16\r\r\133\16\135\31\3\168\18\17\151\3\r\159\3\4\181\18\3\4\3\18\130\4\135\3\131\4\3\4\4\18\134\3\137\18\3\3c\131\27\17\142\18\3\4\4\16\135\18\16\16\18\18\16\16\149\18\16\134\18\16\18\130\16\131\18\16\16@2\5\131\3\16\16\4\4@\3\5\3\18\135\16\4\131@%\26\130\18\3\3\16\16\137\6\18\18\27\27\133\b\29\27\18\25\3\16\16\3\3\4\16\133\18`\31\192E\192\2\b\16\3\16\130\4\3\3\131\16@6\5\130\3\130\16\3\134@_\v\18\134\16\137\6\3\3\130\18\3\25\137`>\0\136@Z\224<\1\0\132\160\130\1\132\3`\29\b\4\4\3\16\16\18\142\16\18\160}\a\25\27\134\16\18\133\3\16\224\190\n\192;\1\4\3\224\191\3\0\134@F\224\192\4\v\29\18\133\b\137\16\3\18\16\133\18\130@z\2\131\18\130`\178\1\16\18`\b@\19\4\130\16\139\18\131@=\5\4\4\130\16\130\4@\200\5\16\16\18\133\16\4A\153\f\130\b\133\29\27\29\132\16\3\130\4\3\135`\188\3\150\18\16\143\128\182\a\3\131\4\16\130\3\16\131`v@\26\0\16`\163\128x\5\134\16\25\134\b\29Ay\0\25\224\&7\0\0\137\224\181\1\1\132\4`\213\b\4\4\3\3\134\16\4\4\133\129G\128:\4\16\18\18\4\139@\201\0\4\129.\2\168\18\3\161\167\128\157\b\18\29\131\16\130\18\4\134\b\192\228\4\136\b\29\133\18`O\5\145\18\130\16\151\18Af\19\18\16\16\134\18\130\16\3\131\16\130\4\130\3\16\3\16\135\4\133@[\20\16\4\4\25\139\16\175\18\3\18\18\134\3\131\16\27\133\18\17\135\3e\170\0\164\129\24\1\132\18@A@\172\6\3\18\18\136\3\18\16@\17\3\17\16\134\3`=\22\131\18\159\16\18\130\29\142\25\29\25\130\29\3\3\133\29\137\6\137\b\29\3@\1\16\21\22\21\22\4\4\135\18\16\163\18\131\16\141\3\4\132C5\19\132\18\138\3\16\163\3\16\135\29\3\133\29\16\29\29\132\25\131\29@j\t\170\18\4\4\131\3\4\133\3\4@\231\b\3\3\18\137\6\133\25\133\18@\n\2\131\18\130@\238\b\18\18\134\4\130\18\131\3\140AC\30\3\3\133\4\3\18\4\137\6\130\4\3\29\29\165\0\16\0\132\16\0\16\16\170\1\25\17\130\1\130\200A\215@\239\0\16\128\b\0\168\128\6\0\160\224\22\6\3\142\18\16\184\128\t\1\194\18B\165\31\136\25\147\b\130\16\143\18\137\29\133\16\213\0\16\16\133\1\16\16\20\132\235\18\29\25\144\18\t\153\18\21\v\22\130\16\202\18\130\25\130\a\135\18\134AY\19\3\4\136\16\146\18\3\3\4\25\25\136\16\145\18\3\3\139\16\140a\160@\t\0\179@\24\2\134\3\135BF\b\138\3\130\25\17\130\25\27\18a\157\14\133\16\137\b\133\16\133\25\20\131\25\130\3\r\3@\16\23\162\18\17\180\18\134\16\132\18\3\3\161\18\3\18\132\16\197\18\137\16\158\18\16BR\3\3\3\130\4b\142\b\133\4\130\3\131\16\29\130\16C\197\0\157a\135\a\138\16\171\18\131\16\153\18A\201\5\b\130\16\161\29\150@uB\175\2\25\25\180C\v\0\134C,\a\3\4\4\135\3\133\4\137C\178`~\21\6\133\16\134\25\17\133\25\16\16\141\3\5\143\3\176\16\131\3\4\174\18A\161B\136@.\1\18\130B\178\b\25\137\29\136\3\136\29\25\25B|\1\157\18A\160A\158\0\130d\189\0\171DaC\6\1\4\130@1\a\16\131\25\163\18\135\4\135`\"\5\130\16\132\25\137\6CL@\169\21\133\17\25\25\136\1\134\16\170\0\16\16\130\0\135\25\135\16\130\3\25\140@\156\3\131\18\3\133BVA\240\r\132\16\171\1\190\17\140\1\17\161\1\164\17\191f\152\224\1\136\0\136\224\"\25\224\1\&7\3\136\1\135\0Bx\a\133\0\16\16\135\1\135\0@\3\224\15\1G\151@\1@\21\1\141\1`!\0\2\192\3\2\132\1\16H\152\5\2\28\1\130\28\130\160\v\4\130\28\131\1\16`\n\n\16\130\28\135\1\132\0\130\28\16\16\192\28\14\28\28\16\138\t\132\r\133\20\25\25\23\24\21\23@\3\31\135\25\n\v\132\r\t\136\25\23\24\131\25\19\19\130\25\26\21\22\138\25\26\25\19\137\25\t\132\r\16\137\f\r\b\17\16\16\133\b\130\26\21\22\17\137`\6\27\16\140\17\130\16\160\27\142\16\140\3\131\5\3\130\5\139\3\142\16\29\29\0\131\29\0\29\29i*\2\130\0\1@\v\5\26\132\0\133\29\0`\1\2\131\0\29H\153\31\131\18\1\29\29\1\1\0\0\132\26\0\131\1\29\26\29\29\1\29\143\b\162\a\0\1\131\a\b\29\29\131\a\16\132\26\132\29\26\26\131@\25@\2\5\134\29\26\158\29\26`\f\a\26\158\29\130\139\26\135\29Db\31\147\29\26\26\134\29\21\22\208\29\26\157\29\152\26\167\29\133\26\196\29\152\16\138\29\148\16\187\b\205\29\149\16\b\129\182\29\26\136\29\26\181\29\135\26\238\29\26\129\247`5\224\1\1\t\157\b\171\29\132\26\21\22\158\26\224\19\1\5\143\26\129\255\29\129@\222\224\1\v\0\190`\23\24\159\26\21\22\130\129\26\175\29\148\26\29\29\133\26\166\29\16\16\159\29\16\232\29\175i\4@\249\129\209i\131\5\1\0\133\1\17\17\233\213\&3\224\1 \2\1\133\29@\6\31\130\3\0\1\132\16\131\25\b\25\25\165\1\16\1\132\16\1\16\16\183\18\134\16\17\25\141\16\3\150\18\136E\6\224\2\f\1\159\3B\14\t\23\24\130\25\23\24\25\23\24\136K\185\0\20@\vB$\161\1\15\132\25\17\137\25\20\20\131\25\20\25\21\140\25\29\29K\227\128\1\23\20\161\16\153\29\16\216\29\139\16\129\213\29\153\16\143\29\t\130\25\29\17\18\a\225=\1\0\29\225\127\0\6\20\21\22\22\29\136\aDU\r\20\132\17\29\29\130\a\17\18\25\29\29\16\213G\19\31\3\28\28\17\17\18\20\217\18\25\130\17\18\132\16\170\18\16\221\18\16\29\29\131\b\137\29\159\18\163\29\138\31\16\29\143\18\158\29\16\137\b\157\29\135\b\29\142\b\159\29\137\b\166\29\142\b\130\191\29\18\179\189\16\18\19\191\29\18\129\163\253\16\149\18\17\136\246\18\130\16\182\29\136\16\167d\137\a\130\139\18\17\130\25\143\18H\204\1\147\16\225M\23\224\1\5\b\18\3\130\5\25\137\3\25\17\224$\19I\225\15\197\18\137\a\3\3\133\25\135\16\150\28\136\17\28\28\224!\4\235J\16\224\1\30\0\17\173\v\224\17\2\128a\0\18\224\\\16\0\132`\2\224\1\5b\152\6\132\16\0\1\16\1\16`\16\v\151\16\130\17\0\1\18\17\17\1\134\18I\163\3\131\18\3\150g\129\28\4\131\29\3\130\16\133\b\29\29\27\29\133\16\179\18\131\25\135\16\4\4\177\18\143\4\3\3\135f\142\b\133\16\145\3\133\18\130\25\18J\236\n\137\6\155\18\135\3\25\25\150\18\138F\252\2\16\25\156IW\134b\166C\20\130\4\140\25\16\17\137\6\131\16\25\25\132\18\3\17\136\18\137\6\132H\217\0\133\134b\3\4\3\3\136I\179\3\135\18\3\4i\2\t\16\131\25\143\18\17\133\18\130\29F\214\1\177\18InI\31G2\n\18\3\18\151\16\18\18\17\25\25\138FI\r\4\4\25\25\18\17\17\4\3\137\16\133\18\16\160\3\194\239\5\170\1\28\131\17\136A0\4\131\16\207\1\162@\222`\2\0\25G4@\199\20\18\215\161\16\18\139\16\150\18\131\16\176\18\131\16\14\134\253\16\14\14@\3\31\135\253\16\14\15\177\253\16\15\130\237\18\16\16\233\18\165\16\134\1\139\16\132\1\132\16\18\3\137\18\26\140i)\202\255\31\235\18\144\28\143\16\130\234\18\22\21\143\29\191\18\16\16\181\18\134\16\29\159\16\139\18\27\130\29\143\3\134O\19\b\133\16\143\3\25\20\20\19\19\227\27\1\128\1\r\25\25\21\22\131\25\130\19\130\25\16\131\25\20\128\19\6\130\25\26\20\130\26\16La\0\131@h\6\129\134\18\16\16\r\16\239_\26\1\21\22@ \n\25\137\18\17\172\18\17\17\158\18\130\225\21\2\0\16j\238\27\27\27\26\28\29\27\27\16\29\131\26\29\29\137\16\130\r\29\29\16\16\139\18\16\153\18\16\146`\205\31\142\18\16\16\141\18\161\16\250\18\132\16\130\25\131\16\172\b\130\16\136\29\180\a\131\b\144\29\b\b\130\29\r\16\140\29\130\16\29\174\16\172\29\3\129\129\16A\237\31\176\18\142\16\3\154\b\131\16\159\18\131\b\136\16\147\18\a\135\18\a\132\16\165\18\132\3\132\16\157\18\16\0\25J\19\f\135\18\25\132\a\169\16\167\0\167\1\205\18\129}\23\163\0\131\16\163\1\131\16\167\18\135\16\179\18\138\16\25\138\0\16\142\0\16\134N\142\a\16\138\1\16\142\1\16\134F\189\23\194\16\130\182\18\136\16\149\18\137\16\135\18\151\16\133\17\16\169\17\16\136\17\196`\211\2\18\16\171\171\207\0\16Ke\15\25\135\b\150\18\29\29\134\b\158\18\135\16\136\b\175\128\212\31\132\16\132\b\149\18\133\b\130\16\25\153\18\132\16\25\191\16\183\18\131\16\b\b\18\18\143\b\16\16\173\bM`\6\3\3\132\16\131\3\131i\211\0\156j\"\14\131\16\3\136\b\134\16\136\25\134\16\156\18\b\bB\217\a\b\159\16\135\18\29\155\18L\237\31\132\b\134\25\136\16\181\18\130\16\134\25\149\18\16\16\135\b\146\18\132\16\135\b\145\18\134\16\131\25\139\16\22\134\b\207\16\200\18\182\16\178\0\140\16\178\1\134\16\133\b\163\18\131\3\135LW\5\165\16\158\b\16\169J;\0\20L4\31\202\16\130\3\156\18\137\b\18\135\16\149\18\138\3\131\b\132\25\149\16\145\18\131\3\131\25\165\16\148\18\134\2\b\147\16E\196\14\4\3\4\180\18\142\3\134\25\131\16\147\b\137\6c\18\1\18\136Cl\0\172l!CH\v\25\25\r\131\25\3\137\16\r\16\16\152m\128\t\133\16\130\3\163\18\132\3\4\135K\195\14\131\25\18\4\4\18\135\16\162\18\3\25\25\18\136I\234\4\175\18\130\4\136C\171\21\18\131\25\131\3\25\4\3\137\6\18\25\18\130\25\16\147\b\138\16\145\18N}i\244J\0\1\133\25@t\0\189\235U\0\0\142L)\5\25\133\16\174\18\3N}\0\132`oC\148\237\177\f\1\3\3C\129\0\131m\173\0\16L\206mh\3\132\16\132\18@\15\0\134JH\4\3\129\138\16\180n\201\0\4@t\2\3\131\18J[\b\25\25\16\25\3\130\18\157\16@\171L5K(JF\2\18\25\18AO\3\129\165\16\174\174\203\160\25\2\150\25\131K^`0j\165D\134\2\25\18\138`\160\4\140\25\146\16\170J\208\0\4`L@\192\4\137\6\181\16\154a\244d\180\2\132\3\131Kd\t\b\130\25\29\134\18\129\184\16\171\129#\b\3\3\25\227\16\159\0\159\1Mu\0\139\128\227\0\16@\6\0\18M6\0\133M\209o(\f\3\18\4\18\4\3\130\25\136\16\137\6\197`&\0\166\160\158\0\3@\186\t\18\25\18\4\154\16\18\137\3\167E\1B\19P^e\r\4\130\3\173\18\140\160\172\4\132\25\140\16\200A\195\2\25\129\245M\212\1\164\18K\225`\177\6\132\25\137\16\137\6\146B\207\0\25L\28\4\149\3\16\4\134mM\2\3\3\200\161p\2\165\18\133O\138N\157\2\134\3\18bw\0\133C1\4\18\18\16\159\18N\129\0\3n\246\0\3b%\1\130\181\140\220\2\4\25\25N\207\v\18\4\140\18\16\161\18\4\4\132\3\130\128%\4\140\25\137\6\213O\158\31\148\b\135\29\131\27\144\29\140\16\25\135\153\18\229\16\238\a\16\132\25\138\16\129\195\18\148\203\16\224\18\25\25\25\140\16\136\175\18\143\r\3\133\18\142\3\159\169\16\132\198\18\195\184\16\132\184\18\134L\227\134\4\0\206@\b\0\133D\26\1\16\132P\28\3\175\18\134\3N\27\3\131\17\25\29@\216\v\16\134\b\16\148\18\132\16\146\18\133\175ao\31\150\b\131\25\228\16\202\18\131\16\3\18\182\4\134\16\131\3\140\17\191\16\17\17\25\17\3\138\16\4\4\141\4\16\18\175\245\16B\n\2\213\18\169Pw\14\18\197\230\16\131\17\16\134\17\16\17\17\16\130\162PU\0\156\143\163\20\141\16\131\18\135\16\131\139\18\146\131\16\234\18\132\16\140\18\130\16\136A\2\28\18\16\16\29\3\3\25\131\r\164\219\16\173\3\16\16\150\3\136\16\243\29\187\16\129\245\29\137\16I\239\1\187\29A\158\29\130\29\133\4\135\r\135\3\29\29\134\3\157\29\131\3\188\29\148\16\193\29\130\3\29\249\16\147\b\139`\3\19\214\29\136\16\152\b\129\134\16\153\0\153\1\153\0\134\1\16\145\1@\nd\214N\184@\6\5\131\0\16\135\0\131G\141\3\134\1\16\138\128\31\0\0@\21\1\16\135e\1\192\14\4\132\0\16\0\130\128\16\128N\224\3\a\v\155\1\16\16\152\0\26\152\1\26\133\1\224\a\23S\249\16\177\6\131\255\29\182\3\131\29\177\3\135\29\3\141\29\3O\150\20\142\16\132\3\16\142\3\136\207\16\137\1\18\147\1\133\16\133\1\129\212O\245\1\144\3C\152B+E\203\a\189\17\160\16\3\239\16\172D\242\2\3\134\17E\197\n\131\16\18\29\130\191\16\157\18\3\144N\158\v\3\137\6\132\16\27\131\207\16\154\18\17@\f\1\133\229B\141R\fd,\17\129\196\18\16\16\136\b\134\3\168\16\161\0\161\1\134\3\17CgB\18\r\134\144\16\186\b\29\130\b\27\131\b\203\16\172I\227\1\129\193@7\0\154\135dCh\0\137\128HD4R\135`\1\0\130\224\30\1\224\1\0`\15\0\131dw\0\131\160\&2\3\137\18\16\144F\206d\134\19\144\18\179\16\26\26\130\141\16\171\29\131\16\227\29\139\16\142\29\16@\3\16\142\29\16\164\29\137\16\140\b\129\160\29\183\16\156\29\140` \25\136\29\134\16\29\29\141\16\133\29\129\153\16\129\250\29\132\28\133\215\29\131\16\144\29\130g\b\r\246\29\131\16\222\29\133\16\139\29\131\16\29\142`\6\3\183\29\135\16PU\5\167\29\135\16\157\29TI\t\205\16\130\211\29\139\16\141\29\16`3@R\a\173\29\16\134\29\135\16\141\160a@\3\6\129\146\29\16\182\29\164Q\210\6\133\16\18\130\205\221\16Qn\1\160\183`\240\1\129\219`\229\1\172\255B\194\2\18\186\174s)\31\132\235\16\18\147\161\16\132\157\18\139\225\16\18\166\200\16\18\132\16\18\160\221\16\18\171\184\208\16\r\157\16\17\223\r\255\16\129\239\3\131\252\143\16\15\131\255\251\16\15\16\160\a"

ucTable :: [(Int, Int, Int)]
ucTable =
  [(97,122,-32),(181,181,743),(224,254,-32),(255,255,121),(257,303,-1),(305,305,-232),(307,382,-1),(383,383,-300),(384,384,195),(387,402,-1),(405,405,97),(409,409,-1),(410,410,163),(414,414,130),(417,445,-1),(447,447,56),(453,453,-1),(454,454,-2),(456,456,-1),(457,457,-2),(459,459,-1),(460,460,-2),(462,476,-1),(477,477,-79),(479,498,-1),(499,499,-2),(501,572,-1),(575,576,10815),(578,591,-1),(592,592,10783),(593,593,10780),(594,594,10782),(595,595,-210),(596,596,-206),(598,599,-205),(601,601,-202),(603,603,-203),(604,604,42319),(608,608,-205),(609,609,42315),(611,611,-207),(613,613,42280),(614,614,42308),(616,616,-209),(617,617,-211),(618,618,42308),(619,619,10743),(620,620,42305),(623,623,-211),(625,625,10749),(626,626,-213),(629,629,-214),(637,637,10727),(640,640,-218),(642,642,42307),(643,643,-218),(647,647,42282),(648,648,-218),(649,649,-69),(650,651,-217),(652,652,-71),(658,658,-219),(669,669,42261),(670,670,42258),(837,837,84),(881,887,-1),(891,893,130),(940,940,-38),(941,943,-37),(945,961,-32),(962,962,-31),(963,971,-32),(972,972,-64),(973,974,-63),(976,976,-62),(977,977,-57),(981,981,-47),(982,982,-54),(983,983,-8),(985,1007,-1),(1008,1008,-86),(1009,1009,-80),(1010,1010,7),(1011,1011,-116),(1013,1013,-96),(1016,1019,-1),(1072,1103,-32),(1104,1119,-80),(1121,1230,-1),(1231,1231,-15),(1233,1327,-1),(1377,1414,-48),(4304,4351,3008),(5112,5117,-8),(7296,7296,-6254),(7297,7297,-6253),(7298,7298,-6244),(7299,7300,-6242),(7301,7301,-6243),(7302,7302,-6236),(7303,7303,-6181),(7304,7304,35266),(7545,7545,35332),(7549,7549,3814),(7566,7566,35384),(7681,7829,-1),(7835,7835,-59),(7841,7935,-1),(7936,8039,8),(8048,8049,74),(8050,8053,86),(8054,8055,100),(8056,8057,128),(8058,8059,112),(8060,8061,126),(8064,8113,8),(8115,8115,9),(8126,8126,-7205),(8131,8131,9),(8144,8161,8),(8165,8165,7),(8179,8179,9),(8526,8526,-28),(8560,8575,-16),(8580,8580,-1),(9424,9449,-26),(11312,11359,-48),(11361,11361,-1),(11365,11365,-10795),(11366,11366,-10792),(11368,11507,-1),(11520,11565,-7264),(42561,42899,-1),(42900,42900,48),(42903,42998,-1),(43859,43859,-928),(43888,43967,-38864),(65345,65370,-32),(66600,66811,-40),(66967,67004,-39),(68800,68850,-64),(71872,93823,-32),(125218,125251,-34)]

lcTable :: [(Int, Int, Int)]
lcTable =
  [(65,222,32),(256,302,1),(304,304,-199),(306,374,1),(376,376,-121),(377,381,1),(385,385,210),(386,388,1),(390,390,206),(391,391,1),(393,394,205),(395,395,1),(398,398,79),(399,399,202),(400,400,203),(401,401,1),(403,403,205),(404,404,207),(406,406,211),(407,407,209),(408,408,1),(412,412,211),(413,413,213),(415,415,214),(416,420,1),(422,422,218),(423,423,1),(425,425,218),(428,428,1),(430,430,218),(431,431,1),(433,434,217),(435,437,1),(439,439,219),(440,444,1),(452,452,2),(453,453,1),(455,455,2),(456,456,1),(458,458,2),(459,494,1),(497,497,2),(498,500,1),(502,502,-97),(503,503,-56),(504,542,1),(544,544,-130),(546,562,1),(570,570,10795),(571,571,1),(573,573,-163),(574,574,10792),(577,577,1),(579,579,-195),(580,580,69),(581,581,71),(582,886,1),(895,895,116),(902,902,38),(904,906,37),(908,908,64),(910,911,63),(913,939,32),(975,975,8),(984,1006,1),(1012,1012,-60),(1015,1015,1),(1017,1017,-7),(1018,1018,1),(1021,1023,-130),(1024,1039,80),(1040,1071,32),(1120,1214,1),(1216,1216,15),(1217,1326,1),(1329,1366,48),(4256,4301,7264),(5024,5103,38864),(5104,5109,8),(7312,7359,-3008),(7680,7828,1),(7838,7838,-7615),(7840,7934,1),(7944,8121,-8),(8122,8123,-74),(8124,8124,-9),(8136,8139,-86),(8140,8140,-9),(8152,8153,-8),(8154,8155,-100),(8168,8169,-8),(8170,8171,-112),(8172,8172,-7),(8184,8185,-128),(8186,8187,-126),(8188,8188,-9),(8486,8486,-7517),(8490,8490,-8383),(8491,8491,-8262),(8498,8498,28),(8544,8559,16),(8579,8579,1),(9398,9423,26),(11264,11311,48),(11360,11360,1),(11362,11362,-10743),(11363,11363,-3814),(11364,11364,-10727),(11367,11371,1),(11373,11373,-10780),(11374,11374,-10749),(11375,11375,-10783),(11376,11376,-10782),(11378,11381,1),(11390,11391,-10815),(11392,42875,1),(42877,42877,-35332),(42878,42891,1),(42893,42893,-42280),(42896,42920,1),(42922,42922,-42308),(42923,42923,-42319),(42924,42924,-42315),(42925,42925,-42305),(42926,42926,-42308),(42928,42928,-42258),(42929,42929,-42282),(42930,42930,-42261),(42931,42931,928),(42932,42946,1),(42948,42948,-48),(42949,42949,-42307),(42950,42950,-35384),(42951,42997,1),(65313,65338,32),(66560,66771,40),(66928,66965,39),(68736,68786,64),(71840,93791,32),(125184,125217,34)]
