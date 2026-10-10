module Mhs.Reflect where
import Data.ByteString.Char8(unpack, pack, ByteString)
import Data.Char(isDigit, isAlpha, isSpace, chr, ord)
import Data.Int(Int64)
import Data.List(stripPrefix)
import Data.Text(Text)
import Data.Text.Encoding(decodeUtf8)
import System.IO.Serialize(writeSerializedBS)
import Mhs.Print

type Ident = String

data Exp
  = Var Ident
  | Lam Ident Exp
  | App Exp Exp
  | Lit Literal
  deriving (Show)

data Literal
  = LInt Int
  | LDbl Double
  | LInt64 Int64
  | LInteger Integer
  | LComb String
  | LFFI String
  | LText Text
  deriving (Show)

data Prog = Prog [(Ident, Exp)] Exp
  deriving (Show)

ppProg :: Prog -> String
ppProg (Prog []  b) = ppExp b
ppProg (Prog ies b) = unlines $
  ["let"] ++ map (\ (i, e) -> "    " ++ i ++ " = " ++ ppExp e) ies ++ ["in  " ++ ppExp b]

ppExp :: Exp -> String
ppExp (Var i) = i
ppExp (Lam i e) = "(\\" ++ i ++ "." ++ ppExp e ++ ")"
ppExp (App f a) = "(" ++ ppExp f ++ " " ++ ppExp a ++ ")"
ppExp (Lit l) = ppLit l

ppLit :: Literal -> String
ppLit (LInt i) = "#" ++ show i
ppLit (LDbl i) = "&" ++ show i
ppLit (LInt64 i) = "#" ++ show i
ppLit (LInteger i) = "%" ++ show i
ppLit (LText s) = show s
ppLit (LComb s) = s
ppLit (LFFI s) = "^" ++ s

isId, isId' :: Char -> Bool
isId c = isAlpha c || c `elem` "+-*/=<>."

parse :: String -> Prog
parse s = p [] [] s
  where
    p :: [(Ident, Exp)] -> [Exp] -> String -> Prog
    p defs stk (' ':s) = p defs stk s
    p defs stk ('\n':s) = p defs stk s
    p defs stk ('#':'#':s) = lit (LInt64 . read) defs stk s
    p defs stk ('#':s) = lit (LInt . read) defs stk s
    p defs stk ('&':s) = lit (LDbl . read) defs stk s
    p defs stk cs@('_':_) = case tok cs of (i, r) -> p defs (Var i : stk) r
    p defs (e:stk) (':':s) =
      case tok s of (i, r) -> p ((i', e):defs) (Var i':stk) r
                              where i' = '_':i
    p defs stk ('"':s) =
      case getByteString s of (bs, r) -> p defs (Lit (LText (decodeUtf8 bs)) : stk) r
    p defs stk ('%':s) = case span (/= '"') s of (i, _:r) -> p defs (Lit (LInteger (read i)) : stk) r
    p defs stk ('^':s) = lit LFFI defs stk s
    p defs (e1:e2:stk) ('@':s) = p defs (App e2 e1 : stk) s
    p defs stk cs@(c:_) | isId c = case tok cs of (c, r) -> p defs (Lit (LComb c) : stk) r
    p defs [e] ('}':_) = Prog defs e
    -- '['  array
    -- '$'  raw ByteString
    -- '!'  Tick
    -- ';'  C function
    p _ _   cs = error $ "parse: unimplemented: " ++ show cs

    lit :: (String -> Literal) -> [(Ident, Exp)] -> [Exp] -> String -> Prog
    lit con defs stk s =
      case tok s of (sn, r) -> p defs (Lit (con sn) : stk) r
    tok = span (not . isSpace)

getByteString :: String -> (ByteString, String)
getByteString = get []
  where
    get r ('"':s) = (pack (reverse r), s)
    get r ('\\':'"':s) = get ('"':r) s
    get r ('\\':'^':s) = get ('^':r) s
    get r ('\\':'|':s) = get ('|':r) s
    get r ('\\':'?':s) = get ('\x7f':r) s
    get r ('\\':'_':s) = get ('\xff':r) s
    get r ('\\':'\\':s) = get ('\\':r) s
    get r ('^' :c:s) | c < '\x40' = get (chr (ord c - 0x20) : r) s
                        | otherwise  = get (chr (ord c + 0x40) : r) s
    get r ('|' :c:s) = get (chr (ord c + 0x80) : r) s
    get r (c:s)     = get (c : r) s
    get _ [] = error "getByteString"

verifyHeader :: String -> Maybe (Int, String)
verifyHeader s =
  case stripPrefix "v8.4\n" s of
    Just r ->
      case span isDigit r of
        (sn@(_:_), '\n':r') -> Just (read sn, r')
        _ -> Nothing
    _ -> Nothing

reflect :: a -> IO Prog
reflect a = do
  bs <- writeSerializedBS a
  let s = unpack bs
      Just (_, s') = verifyHeader s
  putStrLn "-----"
  putStrLn s'
  putStrLn "-----"
  cuprint a
  putStrLn "-----"
  return (parse s')
