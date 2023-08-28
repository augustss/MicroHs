module MicroHs.Lex(lexTop, Token(..), Line, Col, Loc, isLower_, tokensLoc) where
import Prelude --Xhiding(lex, showChar)
import Data.Char
--Ximport Compat
--import Debug.Trace

data Token
  = TIdent  Loc [String] String
  | TString Loc String
  | TChar   Loc Char
  | TInt    Loc Int
  | TSpec   Loc Char
  | TError  Loc String
  | TBrace  Loc
  | TIndent Loc
  --Xderiving (Show)

type Line = Int
type Col  = Int

type Loc = (Line, Col)

lexTop :: String -> [Token]
lexTop = layout [] .
         --take 10 .
         lex 1 1

lex :: Line -> Col -> String -> [Token]
lex l c (' ':cs)  = lex l (c+1) cs
lex l _ ('\n':cs) = tIndent (lex (l+1) 1 cs)
lex l c ('\r':cs) = lex l c cs
lex l c ('{':'-':cs) = skipNest l (c+2) 1 cs
lex l c ('-':'-':cs) | isComm rs = skipLine l (c+2+length ds) cs
  where {
    (ds, rs) = span (eqChar '-') cs;
    isComm [] = True;
    isComm (d:_) = not (isOper d)
    }
lex l c (d:cs) | isLower_ d =
  case span isIdent cs of
    (ds, rs) -> tIdent (l, c) [] (d:ds) (lex l (c + 1 + length ds) rs)
lex l c cs@(d:_) | isUpper d = upperIdent l c c [] cs
lex l c ('-':d:cs) | isDigit d =
  case span isDigit cs of
    (ds, rs) -> TInt (l, c) (readInt ('-':d:ds)) : lex l (c + 2 + length ds) rs
lex l c (d:cs) | isDigit d =
  case span isDigit cs of
    (ds, rs) -> TInt (l, c) (readInt (d:ds)) : lex l (c + 1 + length ds) rs
lex l c (d:cs) | isOper d  =
  case span isOper cs of
    (ds, rs) -> TIdent (l, c) [] (d:ds) : lex l (c + 1 + length ds) rs
lex l c (d:cs) | isSpec d  = TSpec (l, c) d : lex l (c+1) cs
lex l c ('"':cs) =
  let
    loc = (l, c)
  in 
    case takeChars loc (TString loc) '"' 0 [] cs of
      (t, n, rs) -> t : lex l (c + 2 + n) rs
lex l c ('\'':cs) =
  let
    loc = (l, c)
  in
    case takeChars loc (TChar loc . head) '\'' 0 [] cs of  -- XXX head of
      (t, n, rs) -> t : lex l (c + 2 + n) rs
lex l c (d:_) = [TError (l, c) $ "Unrecognized input: " ++ showChar d]
lex _ _ [] = []

-- Skip a { - - } style comment
skipNest :: Line -> Col -> Int -> String -> [Token]
skipNest l c 0 cs = lex l c cs
skipNest l c n ('{':'-':cs) = skipNest l (c+2) (n + 1) cs
skipNest l c n ('-':'}':cs) = skipNest l (c+2) (n - 1) cs
skipNest l _ n ('\n':cs)    = skipNest (l+1) 1 n     cs
skipNest l c n ('\r':cs)    = skipNest l     c n     cs
skipNest l c n (_:cs)       = skipNest l (c+1) n     cs
skipNest l c _ []           = [TError (l, c) "Unclosed {- comment"]

-- Skip a -- style comment
skipLine :: Line -> Col -> String -> [Token]
skipLine l c cs@('\n':_) = lex l c cs
skipLine l c (_:cs)      = skipLine l c cs
skipLine _ _ []          = []

tIndent :: [Token] -> [Token]
tIndent ts@(TIndent _ : _) = ts
tIndent ts = TIndent (tokensLoc ts) : ts

takeChars :: Loc -> (String -> Token) -> Char -> Int -> String -> String -> (Token, Int, String)
takeChars loc _ c n _ [] = (TError loc ("Unmatched " ++ [c]), n, [])
takeChars loc fn c n str ('\\':cs) =
  case decodeChar (n+1) cs of
    (d, m, rs) -> takeChars loc fn c m (d:str) rs
takeChars   _ fn c n str (d:cs) | eqChar c d = (fn (reverse str), n, cs)
takeChars loc fn c n str (d:cs) = takeChars loc fn c (n+1) (d:str) cs

decodeChar :: Int -> String -> (Char, Int, String)
decodeChar n ('n':cs) = ('\n', n+1, cs)
decodeChar n ('r':cs) = ('\r', n+1, cs)
decodeChar n ('t':cs) = ('\t', n+1, cs)
decodeChar n (c  :cs) = (c,    n+1, cs)
decodeChar n []       = ('X',  n,   [])

isOper :: Char -> Bool
isOper c = elemBy eqChar c "@\\=+-:<>.!#$%^&*/|~?"

isSpec :: Char -> Bool
isSpec c = elemBy eqChar c "()[],{}`;"

isIdent :: Char -> Bool
isIdent c = isLower_ c || isUpper c || isDigit c || eqChar c '\''

isLower_ :: Char -> Bool
isLower_ c = isLower c || eqChar c '_'

upperIdent :: Line -> Col -> Col -> [String] -> String -> [Token]
--upperIdent l c qs acs | trace (show (l, c, qs, acs)) False = undefined
upperIdent l c sc qs acs =
  case span isIdent acs of
   (ds, rs) ->
    case rs of
      '.':cs@(d:_) | isUpper d -> upperIdent l (c + 1 + length ds) sc (ds:qs) cs
                   | isLower d -> ident isIdent
                   | isOper  d -> ident isOper
         where {
           ident p =
             case span p cs of
               (xs, ys) -> tIdent (l, sc) (reverse (ds:qs)) xs (lex l (c + 1 + length ds + length xs) ys)
           }
      _ -> TIdent (l, sc) (reverse qs) ds : lex l (c + length ds) rs

tIdent :: Loc -> [String] -> String -> [Token] -> [Token]
tIdent loc qs kw ats | elemBy eqString kw ["let", "where", "do", "of"]
                                 = ti : tBrace ats
                     | otherwise = ti : ats
  where {
    ti = TIdent loc qs kw;

    tBrace ts@(TSpec _ '{' : _) = ts;
    tBrace ts@(TIndent _ : TSpec _ '{' : _) = ts;
    tBrace (TIndent _ : ts) = TBrace (tokensLoc ts) : ts;
    tBrace ts = TBrace (tokensLoc ts) : ts
    }

tokensLoc :: [Token] -> Loc
tokensLoc (TIdent  loc _ _:_) = loc
tokensLoc (TString loc _  :_) = loc
tokensLoc (TChar   loc _  :_) = loc
tokensLoc (TInt    loc _  :_) = loc
tokensLoc (TSpec   loc _  :_) = loc
tokensLoc (TError  loc _  :_) = loc
tokensLoc (TBrace  loc    :_) = loc
tokensLoc (TIndent loc    :_) = loc
tokensLoc []                  = (0,1)

layout :: [Int] -> [Token] -> [Token]
layout mms@(m : ms) tts@(TIndent (_,n)   : ts) | n == m = TSpec (tokensLoc ts) ';' : layout    mms  ts
                                               | n <  m = TSpec (tokensLoc ts) '}' : layout     ms tts
layout          ms      (TIndent _       : ts)          =                            layout     ms  ts
layout mms@(m :  _)     (TBrace (_,n)    : ts) | n > m  = TSpec (tokensLoc ts) '{' : layout (n:mms) ts
layout          []      (TBrace (_,n)    : ts) | n > 0  = TSpec (tokensLoc ts) '{' : layout     [n] ts
layout     (0 : ms)     (t@(TSpec _ '}') : ts)          =                        t : layout     ms  ts 
layout           _      (  (TSpec l '}') :  _)          = TError l "layout error }": []
layout          ms      (t@(TSpec _ '{') : ts)          =                        t : layout  (0:ms) ts
layout          ms      (t               : ts)          =                        t : layout     ms  ts
layout     (_ : ms)     []                              = TSpec (0,0) '}'          : layout     ms  []
layout          []      []                              =                            []
--layout           _      _                             = TError (0,0) "layout error"  : []
