module MicroHs.Lex(
  Token(..), showToken,
  tokensLoc,
  LexState, lexTopLS,
  popLayout,
  ) where
import Prelude hiding(lex)
import Data.Char
import Data.List
import MicroHs.Ident
import Text.ParserComb(TokenMachine(..))
import Compat

data Token
  = TIdent  Loc [String] String  -- identifier
  | TString Loc String           -- String literal
  | TChar   Loc Char             -- Char literal
  | TInt    Loc Integer          -- Integer literal
  | TRat    Loc Rational         -- Rational literal (i.e., decimal number)
  | TSpec   Loc Char             -- one of ()[]{},`;
                                 -- for synthetic {} we use <>, also
                                 --  .  for record selection
                                 --  ~  for lazy
                                 --  !  for strict
                                 --  NOT YET  @  for type app
  | TError  Loc String           -- lexical error
  | TBrace  Loc                  -- {n} in the Haskell report
  | TIndent Loc                  -- <n> in the Haskell report
  | TEnd
  | TRaw [Token]
  deriving (Show)

showToken :: Token -> String
showToken (TIdent _ ss s) = intercalate "." (ss ++ [s])
showToken (TString _ s) = show s
showToken (TChar _ c) = show c
showToken (TInt _ i) = show i
showToken (TRat _ d) = show d
showToken (TSpec _ c) | c == '<' = "{ layout"
                      | c == '>' = "} layout"
                      | otherwise = [c]
showToken (TError _ s) = s
showToken (TBrace _) = "TBrace"
showToken (TIndent _) = "TIndent"
showToken TEnd = "EOF"
showToken (TRaw _) = "TRaw"

incrLine :: Loc -> Loc
incrLine (l, _) = (l+1, 1)

addCol :: Loc -> Int -> Loc
addCol (l, c) i = (l, c + i)

tabCol :: Loc -> Loc
tabCol (l, c) = (l, ((c + 7) `quot` 8) * 8)

mkLoc :: Line -> Col -> Loc
mkLoc l c = (l, c)

mkLocEOF :: Loc
mkLocEOF = (-1,0)

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
lex loc ('.':cs@(d:_)) | isLower_ d =
  TSpec loc '.' : lex (addCol loc 1) cs
lex loc (c:cs@(d:_)) | (c == '!' || c == '~') && (d == '(' || isIdentChar d) =
  TSpec loc c : lex (addCol loc 1) cs
lex loc (d:cs) | isOperChar d =
  case span isOperChar cs of
    (ds, rs) -> TIdent loc [] (d:ds) : lex (addCol loc $ 1 + length ds) rs
lex loc (d:cs) | isSpec d  = TSpec loc d : lex (addCol loc 1) cs
lex loc ('"':cs) =
  case takeChars loc (TString loc) '"' (addCol loc 1) [] cs of
    (t, loc', rs) -> t : lex loc' rs
lex loc ('\'':cs) =
  let tchar [c] = TChar loc c
      tchar _ = TError loc "Illegal Char literal"
  in  case takeChars loc tchar '\'' (addCol loc 1) [] cs of  -- XXX head of
        (t, loc', rs) -> t : lex loc' rs
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
               let i = read ds
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
skipNest loc n ('\t':cs)    = skipNest (tabCol loc)    n      cs
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

takeChars :: Loc -> (String -> Token) -> Char -> Loc -> String -> String -> (Token, Loc, String)
takeChars oloc  _ c loc _ [] = (TError oloc ("Unmatched " ++ [c]), loc, [])
takeChars oloc fn c loc str ('\\':cs) =
  let skipGap l (' ' :rs) = skipGap (addCol l 1) rs
      skipGap l ('\n':rs) = skipGap (incrLine l) rs
      skipGap l ('\r':rs) = skipGap l rs
      skipGap l ('\t':rs) = skipGap (tabCol l) rs
      skipGap l ('\\':rs) = takeChars oloc fn c (addCol l 1) str rs
      skipGap l       rs  = (TError oloc "Bad string gap", l, rs)
  in
  case cs of
    '&':rs -> takeChars oloc fn c (addCol loc 2) str rs
    d:_ | isSpace d -> skipGap loc cs
    _ ->
      case decodeChar cs of
        (d, m, rs) -> takeChars oloc fn c (addCol loc m) (d:str) rs
takeChars   _  fn c loc str (d:cs) | c == d = (fn (reverse str), addCol loc 1, cs)
takeChars oloc fn c loc str (d:cs) = takeChars oloc fn c (addCol loc 1) (d:str) cs

decodeChar :: String -> (Char, Int, String)
decodeChar ('n':cs) = ('\n', 1, cs)
decodeChar ('a':cs) = ('\a', 1, cs)
decodeChar ('b':cs) = ('\b', 1, cs)
decodeChar ('f':cs) = ('\f', 1, cs)
decodeChar ('r':cs) = ('\r', 1, cs)
decodeChar ('t':cs) = ('\t', 1, cs)
decodeChar ('v':cs) = ('\v', 1, cs)
decodeChar ('x':cs) = conv 16 1 0 cs
decodeChar ('o':cs) = conv 8 1 0 cs
decodeChar ('^':c:cs) | '@' <= c && c <= '_' = (chr (ord c - ord '@'), 2, cs)
decodeChar (cs@(c:_)) | isDigit c = conv 10 0 0 cs
decodeChar (c1:c2:c3:cs) | Just c <- lookup [c1,c2,c3] ctlCodes = (c, 3, cs)
decodeChar (c1:c2:cs) | Just c <- lookup [c1,c2] ctlCodes = (c, 2, cs)
decodeChar (c  :cs) = (c,    1, cs)
decodeChar []       = ('X',  0,   [])

-- Nobody uses these, but it's part of the Haskell Report so...
ctlCodes :: [(String, Char)]
ctlCodes =
  [("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX'),
   ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL'),
   ("BS",  '\BS'),  ("HT",  '\HT'),  ("LF",  '\LF'),  ("VT", '\VT'),
   ("FF",  '\FF'),  ("CR",  '\CR'),  ("SO",  '\SO'),  ("SI", '\SI'), 
   ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3'),
   ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB'),
   ("CAN", '\CAN'), ("EM",  '\EM'),  ("SUB", '\SUB'), ("ESC", '\ESC'),
   ("FS",  '\FS'),  ("GS",  '\GS'),  ("RS",  '\RS'),  ("US",  '\US'), 
   ("SP",  '\SP'),  ("DEL", '\DEL')]

conv :: Int -> Int -> Int -> String -> (Char, Int, String)
conv b k r (c:ds) | isHexDigit c, let { n = digitToInt c }, n < b = conv b (k+1) (r * b + n) ds
conv _ k r ds = (chr r, k, ds)

isSpec :: Char -> Bool
isSpec c = elem c specChars
  where specChars :: String
        specChars = "()[],{}`;"

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
  where
    ti = TIdent loc qs kw

    tBrace ts@(TSpec _ '{' : _) = ts
    tBrace ts@(TIndent _ : TSpec _ '{' : _) = ts
    tBrace (TIndent _ : ts) = TBrace (tokensLoc ts) : ts
    tBrace ts = TBrace (tokensLoc ts) : ts

tokensLoc :: [Token] -> Loc
tokensLoc (TIdent  loc _ _:_) = loc
tokensLoc (TString loc _  :_) = loc
tokensLoc (TChar   loc _  :_) = loc
tokensLoc (TInt    loc _  :_) = loc
tokensLoc (TRat    loc _  :_) = loc
tokensLoc (TSpec   loc _  :_) = loc
tokensLoc (TError  loc _  :_) = loc
tokensLoc (TBrace  loc    :_) = loc
tokensLoc (TIndent loc    :_) = loc
tokensLoc _                   = mkLocEOF

readHex :: String -> Integer
readHex = foldl (\ r c -> r * 16 + toInteger (digitToInt c)) 0

-- | This is the magical layout resolver, straight from the Haskell report.
-- https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3
-- The first argument to layoutLS is the input token stream.
-- The second argument is a stack of "layout contexts" (indentations) where a synthetic '{' has been inserted.
-- In the report this is a list-to-list function, but it's encoded differently here.
-- The function returns a the next token, and the state of the layout conversion.
-- The reason is that to implement the Note 5 rule we need to manipulate the state,
-- namely to pop the context stack.  And this has to be initiated from the parser.
-- There are 3 commands that the state can be given:
--   Next  generate the next token (and new state)
--   Pop   pop the context stack
--   Raw   return the rest of the tokens, unprocessed

newtype LexState = LS (Cmd -> (Token, LexState))

data Cmd = Next | Raw | Pop

layoutLS ::                [Token] ->    [Int] -> Cmd      -> (Token,                    LexState)
layoutLS                        ts           ms  Raw        = (TRaw ts,                  LS $ layoutLS  ts     ms )
layoutLS                        ts          mms  Pop        =                                                    
                                                   case (mms, ts) of                                              
                                                     (m:ms,_:_) | m/=0 -> (       TEnd,  LS $ layoutLS  ts     ms )
                                                     _ ->     (TError l "syntax error",  LS $ layoutLS  []     [] ) where l = tokensLoc ts
layoutLS tts@(TIndent x       : ts) mms@(m : ms) _ | n == m = (TSpec (tokensLoc ts) ';', LS $ layoutLS  ts    mms )
                                                   | n <  m = (TSpec (tokensLoc ts) '>', LS $ layoutLS tts     ms ) where {n = getCol x}
layoutLS     (TIndent _       : ts)          ms  _          =                                 layoutLS  ts     ms  Next
layoutLS     (TBrace x        : ts) mms@(m :  _) _ | n > m  = (TSpec (tokensLoc ts) '<', LS $ layoutLS  ts (n:mms)) where {n = getCol x}
layoutLS     (TBrace x        : ts)          []  _ | n > 0  = (TSpec (tokensLoc ts) '<', LS $ layoutLS  ts     [n]) where {n = getCol x}
layoutLS     (TBrace x        : ts)          ms  _          = (TSpec (tokensLoc ts) '<', LS $ layoutLS  (TSpec (tokensLoc ts) '>' : TIndent x : ts) ms)
layoutLS     (t@(TSpec _ '}') : ts)     (0 : ms) _          = (                       t, LS $ layoutLS  ts     ms )
layoutLS     (  (TSpec l '}') :  _)           _  _          = (TError l "layout error }",LS $ layoutLS  []     [] )
layoutLS     (t@(TSpec _ '{') : ts)          ms  _          = (                       t, LS $ layoutLS  ts  (0:ms))
layoutLS     (t               : ts)          ms  _          = (                       t, LS $ layoutLS  ts     ms )
layoutLS     []                         (_ : ms) _          = (TSpec mkLocEOF '>'      , LS $ layoutLS  []     ms )
layoutLS     []                              []  _          = (TEnd                    , LS $ layoutLS  []     [] )

instance TokenMachine LexState Token where
  tmNextToken (LS f) = f Next
  tmRawTokens (LS f) =
    case f Raw of
      (TRaw ts, _) -> ts
      _            -> undefined

-- Used for Note 5.
popLayout :: LexState -> LexState
popLayout (LS f) = snd (f Pop)

lexTopLS :: String -> LexState
lexTopLS s = LS $ layoutLS (lex (mkLoc 1 1) s) []
