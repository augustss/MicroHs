module MicroHs.Lex(
  lexTop,
  Token(..), showToken,
  tokensLoc) where
import Prelude hiding(lex)
import Data.Char
import Data.List
import Compat
import MicroHs.Ident

data Token
  = TIdent  Loc [String] String
  | TString Loc String
  | TChar   Loc Char
  | TInt    Loc Integer
  | TRat    Loc Rational
  | TSpec   Loc Char
  | TError  Loc String
  | TBrace  Loc
  | TIndent Loc
  | TSelect Loc String
--  deriving (Show)

showToken :: Token -> String
showToken (TIdent _ ss s) = intercalate "." (ss ++ [s])
showToken (TString _ s) = show s
showToken (TChar _ c) = show c
showToken (TInt _ i) = show i
showToken (TRat _ d) = show d
showToken (TSpec _ c) = [c]
showToken (TError _ s) = "ERROR " ++ s
showToken (TBrace _) = "TBrace"
showToken (TIndent _) = "TIndent"
showToken (TSelect _ s) = "." ++ s

incrLine :: Loc -> Loc
incrLine (l, _) = (l+1, 1)

addCol :: Loc -> Int -> Loc
addCol (l, c) i = (l, c + i)

tabCol :: Loc -> Loc
tabCol (l, c) = (l, ((c + 7) `quot` 8) * 8)

mkLoc :: Line -> Col -> Loc
mkLoc l c = (l, c)

getCol :: Loc -> Col
getCol (_, c) = c

--getLin :: Loc -> Col
--getLin (l, _) = l

{-  This is slower and allocates more.
    It needs some strictness, probably
type Loc = Int

incrLine :: Loc -> Loc
incrLine l = (quot l 1000000 + 1) * 1000000 + 1

addCol :: Loc -> Int -> Loc
addCol loc i = loc + i

mkLoc :: Line -> Col -> Loc
mkLoc l c = l * 1000000 + c

getCol :: Loc -> Col
getCol loc = rem loc 1000000

getLin :: Loc -> Line
getLin loc = quot loc 1000000
-}

---------

lexTop :: String -> [Token]
lexTop = layout [] .
         lex (mkLoc 1 1)

-- | Take a location and string and produce a list of tokens
lex :: Loc -> String -> [Token]
lex loc (' ':cs)  = lex (addCol loc 1) cs
lex loc ('\n':cs) = tIndent (lex (incrLine loc) cs)
lex loc ('\r':cs) = lex loc cs
lex loc ('\t':cs) = lex (tabCol loc) cs  -- TABs are a dubious feature, but easy to support
lex loc ('{':'-':cs) = skipNest (addCol loc 2) 1 cs
lex loc ('-':'-':cs) | isComm rs = skipLine (addCol loc $ 2+length ds) cs
  where
    (ds, rs) = span (== '-') cs
    isComm [] = True
    isComm (d:_) = not (isOperChar d)
lex loc (d:cs) | isLower_ d =
  case span isIdentChar cs of
    (ds, rs) -> tIdent loc [] (d:ds) (lex (addCol loc $ 1 + length ds) rs)
lex loc cs@(d:_) | isUpper d = upperIdent loc loc [] cs
lex loc ('0':x:cs) | toLower x == 'x' = hexNumber loc cs
lex loc cs@(d:_) | isDigit d = number loc cs
lex loc ('.':d:cs) | isLower_ d =
  case span isIdentChar cs of
    (ds, rs) -> TSelect loc (d:ds) : lex (addCol loc $ 1 + length ds) rs
lex loc (d:cs) | isOperChar d  =
  case span isOperChar cs of
    (ds, rs) -> TIdent loc [] (d:ds) : lex (addCol loc $ 1 + length ds) rs
lex loc (d:cs) | isSpec d  = TSpec loc d : lex (addCol loc 1) cs
lex loc ('"':cs) =
  case takeChars loc (TString loc) '"' 0 [] cs of
    (t, n, rs) -> t : lex (addCol loc $ 2 + n) rs
lex loc ('\'':cs) =
  let tchar [c] = TChar loc c
      tchar _ = TError loc "Illegal Char literal"
  in  case takeChars loc tchar '\'' 0 [] cs of  -- XXX head of
        (t, n, rs) -> t : lex (addCol loc $ 2 + n) rs
lex loc (d:_) = [TError loc $ "Unrecognized input: " ++ show d]
lex _ [] = []

hexNumber :: Loc -> String -> [Token]
hexNumber loc cs =
  case span isHexDigit cs of
    (ds, rs) -> TInt loc (readHex ds) : lex (addCol loc $ length ds + 2) rs

number :: Loc -> String -> [Token]
number loc cs =
  case span isDigit cs of
    (ds, rs) | null rs || not (head rs == '.') || (take 2 rs) == ".." ->
               let i = readInteger ds
               in  TInt loc i : lex (addCol loc $ length ds) rs
             | otherwise ->
               case span isDigit (tail rs) of
                 (ns, rs') ->
                   let s = ds ++ '.':ns
                       mkD x r = TRat loc (readRational x) : lex (addCol loc $ length x) r
                   in  case expo rs' of
                         Nothing -> mkD s rs'
                         Just (es, rs'') -> mkD (s ++ es) rs''
  where
    expo (e:'-':xs@(d:_)) | toLower e == 'e' && isDigit d = Just ('e':'-':as, bs) where (as, bs) = span isDigit xs
    expo (e:'+':xs@(d:_)) | toLower e == 'e' && isDigit d = Just ('e':'+':as, bs) where (as, bs) = span isDigit xs
    expo (e:    xs@(d:_)) | toLower e == 'e' && isDigit d = Just ('e':    as, bs) where (as, bs) = span isDigit xs
    expo _ = Nothing

-- Skip a {- -} style comment
skipNest :: Loc -> Int -> String -> [Token]
skipNest loc 0 cs           = lex loc cs
skipNest loc n ('{':'-':cs) = skipNest (addCol loc 2) (n + 1) cs
skipNest loc n ('-':'}':cs) = skipNest (addCol loc 2) (n - 1) cs
skipNest loc n ('\n':cs)    = skipNest (incrLine loc)  n      cs
skipNest loc n ('\r':cs)    = skipNest loc             n      cs
skipNest loc n (_:cs)       = skipNest (addCol loc 1)  n      cs
skipNest loc _ []           = [TError loc "Unclosed {- comment"]

-- Skip a -- style comment
skipLine :: Loc -> String -> [Token]
skipLine loc cs@('\n':_) = lex loc cs
skipLine loc (_:cs)      = skipLine loc cs
skipLine   _ []          = []

-- | Takes a list of tokens and produces a list of tokens. If the first token in
-- the input list is a TIndent, the input is returned unaltered. Otherwise, a
-- TIndent is prepended to the input list        
tIndent :: [Token] -> [Token]
tIndent ts@(TIndent _ : _) = ts
tIndent ts = TIndent (tokensLoc ts) : ts

takeChars :: Loc -> (String -> Token) -> Char -> Int -> String -> String -> (Token, Int, String)
takeChars loc _ c n _ [] = (TError loc ("Unmatched " ++ [c]), n, [])
takeChars loc fn c n str ('\\':cs) =
  case decodeChar (n+1) cs of
    (d, m, rs) -> takeChars loc fn c m (d:str) rs
takeChars   _ fn c n str (d:cs) | c == d = (fn (reverse str), n, cs)
takeChars loc fn c n str (d:cs) = takeChars loc fn c (n+1) (d:str) cs

decodeChar :: Int -> String -> (Char, Int, String)
decodeChar n ('n':cs) = ('\n', n+1, cs)
decodeChar n ('r':cs) = ('\r', n+1, cs)
decodeChar n ('t':cs) = ('\t', n+1, cs)
decodeChar n ('b':cs) = ('\b', n+1, cs)
decodeChar n (c  :cs) = (c,    n+1, cs)
decodeChar n []       = ('X',  n,   [])

isSpec :: Char -> Bool
isSpec c = elem c "()[],{}`;"

upperIdent :: Loc -> Loc -> [String] -> String -> [Token]
--upperIdent l c qs acs | trace (show (l, c, qs, acs)) False = undefined
upperIdent loc sloc qs acs =
  case span isIdentChar acs of
   (ds, rs) ->
    case rs of
      '.':cs@(d:_) | isUpper d    -> upperIdent (addCol loc $ 1 + length ds) sloc (ds:qs) cs
                   | isLower d    -> ident isIdentChar
                   | isOperChar d -> ident isOperChar
         where {
           ident p =
             case span p cs of
               (xs, ys) -> tIdent sloc (reverse (ds:qs)) xs (lex (addCol loc $ 1 + length ds + length xs) ys)
           }
      _ -> TIdent sloc (reverse qs) ds : lex (addCol loc $ length ds) rs

tIdent :: Loc -> [String] -> String -> [Token] -> [Token]
tIdent loc qs kw ats | elem kw ["let", "where", "do", "of"]
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
tokensLoc (TRat    loc _ : _) = loc
tokensLoc (TSpec   loc _  :_) = loc
tokensLoc (TError  loc _  :_) = loc
tokensLoc (TBrace  loc    :_) = loc
tokensLoc (TIndent loc    :_) = loc
tokensLoc (TSelect loc _  :_) = loc
tokensLoc []                  = mkLoc 0 1

-- | This is the magical layout resolver, straight from the Haskell report.
layout :: [Int] -> [Token] -> [Token]
layout mms@(m : ms) tts@(TIndent x       : ts) | n == m = TSpec (tokensLoc ts) ';' : layout    mms  ts
                                               | n <  m = TSpec (tokensLoc ts) '}' : layout     ms tts where {n = getCol x}
layout          ms      (TIndent _       : ts)          =                            layout     ms  ts
layout mms@(m :  _)     (TBrace x        : ts) | n > m  = TSpec (tokensLoc ts) '{' : layout (n:mms) ts where {n = getCol x}
layout          []      (TBrace x        : ts) | n > 0  = TSpec (tokensLoc ts) '{' : layout     [n] ts where {n = getCol x}
layout     (0 : ms)     (t@(TSpec _ '}') : ts)          =                        t : layout     ms  ts 
layout           _      (  (TSpec l '}') :  _)          = TError l "layout error }": []
layout          ms      (t@(TSpec _ '{') : ts)          =                        t : layout  (0:ms) ts
layout          ms      (t               : ts)          =                        t : layout     ms  ts
layout     (_ : ms)     []                              = TSpec (mkLoc 0 0) '}'    : layout     ms  []
layout          []      []                              =                            []

readHex :: String -> Integer
readHex = foldl (\ r c -> r * 16 + toInteger (digitToInt c)) 0
