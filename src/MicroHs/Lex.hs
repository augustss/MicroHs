module MicroHs.Lex(
  Token(..), showToken,
  tokensLoc,
  LexState, lexTopLS,
  popLayout, lex,
  readInt,
  ) where
import Prelude(); import MHSPrelude hiding(lex)
import Data.Char
import Data.List
import MicroHs.Ident
import Text.ParserComb(TokenMachine(..))

data Token
  = TIdent  SLoc [String] String  -- identifier
  | TString SLoc String           -- String literal
  | TChar   SLoc Char             -- Char literal
  | TInt    SLoc Integer          -- Integer literal
  | TRat    SLoc Rational         -- Rational literal (i.e., decimal number)
  | TSpec   SLoc Char             -- one of ()[]{},`;
                                  -- for synthetic {} we use <>, also
                                  --  .  for record selection
                                  --  ~  for lazy
                                  --  !  for strict
                                  --  NOT YET  @  for type app
  | TError  SLoc String           -- lexical error
  | TBrace  SLoc                  -- {n} in the Haskell report
  | TIndent SLoc                  -- <n> in the Haskell report
  | TPragma SLoc String           -- a {-# PRAGMA #-}
  | TEnd    SLoc
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
showToken (TPragma _ s) = "{-# " ++ s ++ " #-}"
showToken (TEnd _) = "EOF"
showToken (TRaw _) = "TRaw"

incrLine :: SLoc -> SLoc
incrLine (SLoc f l _) = let l' = l+1 in seq l' (SLoc f l' 1)

addCol :: SLoc -> Int -> SLoc
addCol (SLoc f l c) i = let c' = c+i in seq c' (SLoc f l c')

tabCol :: SLoc -> SLoc
tabCol (SLoc f l c) = SLoc f l (((c + 7) `quot` 8) * 8)

mkLocEOF :: SLoc
mkLocEOF = SLoc "" (-1) 0

getCol :: SLoc -> Col
getCol (SLoc _ _ c) = c

---------

-- | Take a location and string and produce a list of tokens
lex :: SLoc -> String -> [Token]
lex loc (' ':cs)  = lex (addCol loc 1) cs
lex loc ('\n':cs) = tIndent (lex (incrLine loc) cs)
lex loc ('\r':cs) = lex loc cs
lex loc ('\t':cs) = lex (tabCol loc) cs  -- TABs are a dubious feature, but easy to support
lex loc ('{':'-':cs) = nested (addCol loc 2) cs
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
                   | toLower x == 'o' = octNumber loc cs
                   | toLower x == 'b' = binNumber loc cs
lex loc cs@(d:_) | isDigit d = number loc cs
lex loc ('.':cs@(d:_)) | isLower_ d =
  TSpec loc '.' : lex (addCol loc 1) cs
-- Recognize #line 123 "file/name.hs"
lex loc ('#':xcs) | (SLoc _ _ 1) <- loc, Just cs <- stripPrefix "line " xcs =
  case span (/= '\n') cs of
    (line, rs) ->        -- rs will contain the '\n', so subtract 1 below
      let ws = words line
          file = tail $ init $ ws!!1   -- strip the initial and final '"' 
          loc' = SLoc file (readInt (ws!!0) - 1) 1
      in  lex loc' rs
lex loc ('!':' ':cs) =  -- ! followed by a space is always an operator
  TIdent loc [] "!" : lex (addCol loc 2) cs
lex loc (c:cs@(d:_)) | isSpecSing c && not (isOperChar d) = -- handle reserved
  TSpec loc c :
    let ts = lex (addCol loc 1) cs
    in  if c == '\\' then tLam ts else ts
lex loc (d:cs) | isOperChar d =
  case span isOperChar cs of
    (ds, rs) -> TIdent loc [] (d:ds) : lex (addCol loc $ 1 + length ds) rs
lex loc (d:cs) | isSpec d  = TSpec loc d : lex (addCol loc 1) cs
lex loc ('"':cs) = lexLitStr loc (addCol loc 1) (TString loc) isDQuote cs
  where isDQuote ('"':_) = Just 1
        isDQuote _ = Nothing
lex loc ('\'':cs) = lexLitStr loc (addCol loc 1) tchar isSQuote cs
  where isSQuote ('\'':_) = Just 1
        isSQuote _ = Nothing
        tchar [c] = TChar loc c
        tchar _   = TError loc $ "Illegal Char literal"

lex loc (d:_) = [TError loc $ "Unrecognized input: " ++ show d]
lex loc [] = [TEnd loc]

nested :: SLoc -> [Char] -> [Token]
nested loc ('#':cs) = pragma loc cs
nested loc cs = skipNest loc 1 cs

hexNumber :: SLoc -> String -> [Token]
hexNumber loc cs =
  case span isHexDigit cs of
    (ds, rs) -> TInt loc (readBase 16 ds) : lex (addCol loc $ length ds + 2) rs

octNumber :: SLoc -> String -> [Token]
octNumber loc cs =
  case span isOctDigit cs of
    (ds, rs) -> TInt loc (readBase 8 ds) : lex (addCol loc $ length ds + 2) rs

binNumber :: SLoc -> String -> [Token]
binNumber loc cs =
  case span isBinDigit cs of
    (ds, rs) -> TInt loc (readBase 2 ds) : lex (addCol loc $ length ds + 2) rs
  where isBinDigit c = c == '0' || c == '1'

number :: SLoc -> String -> [Token]
number loc cs =
  case span isDigit cs of
    (ds, rs) | null rs || not (head rs == '.') || (take 2 rs) == ".." ->
               case expo rs of
                 Nothing -> TInt loc (readBase 10 ds) : lex (addCol loc $ length ds) rs
                 Just (es, rs') -> mkD (ds ++ es) rs'
             | otherwise ->
               case span isDigit (tail rs) of
                 (ns, rs') ->
                   let s = ds ++ '.':ns
                   in  case expo rs' of
                         Nothing -> mkD s rs'
                         Just (es, rs'') -> mkD (s ++ es) rs''
  where
    expo (e:'-':xs@(d:_)) | toLower e == 'e' && isDigit d = Just ('e':'-':as, bs) where (as, bs) = span isDigit xs
    expo (e:'+':xs@(d:_)) | toLower e == 'e' && isDigit d = Just ('e':'+':as, bs) where (as, bs) = span isDigit xs
    expo (e:    xs@(d:_)) | toLower e == 'e' && isDigit d = Just ('e':    as, bs) where (as, bs) = span isDigit xs
    expo _ = Nothing
    mkD x r = TRat loc (readRational x) : lex (addCol loc $ length x) r

-- Skip a {- -} style comment
skipNest :: SLoc -> Int -> String -> [Token]
skipNest loc 0 cs           = lex loc cs
skipNest loc n ('{':'-':cs) = skipNest (addCol loc 2) (n + 1) cs
skipNest loc n ('-':'}':cs) = skipNest (addCol loc 2) (n - 1) cs
skipNest loc n ('\n':cs)    = skipNest (incrLine loc)  n      cs
skipNest loc n ('\t':cs)    = skipNest (tabCol loc)    n      cs
skipNest loc n ('\r':cs)    = skipNest loc             n      cs
skipNest loc n (_:cs)       = skipNest (addCol loc 1)  n      cs
skipNest loc _ []           = [TError loc "Unclosed {- comment"]

-- Skip a -- style comment
skipLine :: SLoc -> String -> [Token]
skipLine loc cs@('\n':_) = lex loc cs
skipLine loc (_:cs)      = skipLine loc cs
skipLine loc []          = lex loc []

-- | Takes a list of tokens and produces a list of tokens. If the first token in
-- the input list is a TIndent, the input is returned unaltered. Otherwise, a
-- TIndent is prepended to the input list        
tIndent :: [Token] -> [Token]
tIndent ts@(TIndent _ : _) = ts
tIndent ts = TIndent (tokensLoc ts) : ts

lexLitStr :: SLoc -> SLoc -> (String -> Token) -> (String -> Maybe Int) -> String -> [Token]
lexLitStr oloc loc mk end acs = loop loc [] acs
  where loop l rs cs | Just k <- end cs   = mk (decodeEscs $ reverse rs) : lex (addCol l k) (drop k cs)
        loop l rs ('\\':c:cs) | isSpace c = remGap l rs cs
        loop l rs ('\\':'^':'\\':cs)      = loop (addCol l 3) ('\\':'^':'\\':rs) cs  -- special hack for unescaped \
        loop l rs ('\\':cs)               = loop' (addCol l 1) ('\\':rs) cs
        loop l rs       cs                = loop' l rs cs

        loop' l rs ('\n' :cs) = loop  (incrLine l) ( '\n':rs) cs
        loop' l rs ('\t' :cs) = loop  (tabCol   l) ( '\t':rs) cs
        loop' l rs ('\r' :cs) = loop            l         rs  cs
        loop' l rs     (c:cs) = loop  (addCol l 1) (    c:rs) cs
        loop' _ _          [] = [TError oloc "unterminated Char/String literal"]
--        foo xs = trace (show ("foo", loc, take 20 acs, xs)) xs

        remGap l rs ('\\':cs) = loop   (addCol l 1)       rs  cs
        remGap l rs ('\n':cs) = remGap (incrLine l) ('\n':rs) cs 
        remGap l rs ('\t':cs) = remGap (tabCol   l) ('\t':rs) cs
        remGap l rs ('\r':cs) = remGap           l        rs  cs
        remGap l rs (' ' :cs) = remGap (addCol l 1)       rs  cs
        remGap _ _         _  = error "bad string gap"

decodeEscs :: String -> String
decodeEscs [] = []
decodeEscs ('\\':cs) = decodeEsc cs
decodeEscs (c:cs) = c : decodeEscs cs

decodeEsc :: String -> String
decodeEsc ('n':cs) = '\n' : decodeEscs cs
decodeEsc ('a':cs) = '\a' : decodeEscs cs
decodeEsc ('b':cs) = '\b' : decodeEscs cs
decodeEsc ('f':cs) = '\f' : decodeEscs cs
decodeEsc ('r':cs) = '\r' : decodeEscs cs
decodeEsc ('t':cs) = '\t' : decodeEscs cs
decodeEsc ('v':cs) = '\v' : decodeEscs cs
decodeEsc ('&':cs) = decodeEscs cs
decodeEsc ('x':cs) = conv 16 0 cs
decodeEsc ('o':cs) = conv 8 0 cs
decodeEsc ('^':c:cs) | '@' <= c && c <= '_' = chr (ord c - ord '@') : decodeEscs cs
decodeEsc (cs@(c:_)) | isDigit c = conv 10 0 cs
decodeEsc (c1:c2:c3:cs) | Just c <- lookup [c1,c2,c3] ctlCodes = c : decodeEscs cs
decodeEsc (c1:c2:cs) | Just c <- lookup [c1,c2] ctlCodes = c : decodeEscs cs
decodeEsc (c  :cs) = c : decodeEscs cs
decodeEsc []       = error "Bad \\ escape"


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

conv :: Int -> Int -> String -> String
conv b r (c:ds) | isHexDigit c, let { n = digitToInt c }, n < b = conv b (r * b + n) ds
conv _ r ds = chr r : decodeEscs ds

-- These characters are single characters token, no matter what.
isSpec :: Char -> Bool
isSpec '(' = True
isSpec ')' = True
isSpec '[' = True
isSpec ']' = True
isSpec '{' = True
isSpec '}' = True
isSpec ',' = True
isSpec ';' = True
isSpec '`' = True
isSpec _ = False

-- These characters are single characters token,
-- if not part of an operator.
isSpecSing :: Char -> Bool
isSpecSing '=' = True
isSpecSing '|' = True
isSpecSing '\\' = True
isSpecSing '@' = True
isSpecSing '!' = True
isSpecSing '~' = True
isSpecSing _ = False

upperIdent :: SLoc -> SLoc -> [String] -> String -> [Token]
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

-- For LambdaCase
tLam :: [Token] -> [Token]
tLam (t@(TIdent _ [] "case") : ts) = t : tBrace ts
tLam ts = ts

tIdent :: SLoc -> [String] -> String -> [Token] -> [Token]
tIdent loc qs kw ats | elem kw ["let", "where", "do", "of"]
                       || kw == "if" && isBar ats                -- For MultiWayIf
                                 = ti : tBrace ats
                     | otherwise = ti : ats
  where
    isBar (TSpec _ '|' : _) = True
    isBar _ = False

    ti = TIdent loc qs kw

tBrace :: [Token] -> [Token]
tBrace ts@(TSpec _ '{' : _) = ts
tBrace ts@(TIndent _ : TSpec _ '{' : _) = ts
tBrace (TIndent _ : ts) = TBrace (tokensLoc ts) : ts
tBrace ts = TBrace (tokensLoc ts) : ts

tokensLoc :: [Token] -> SLoc
tokensLoc (TIdent  loc _ _:_) = loc
tokensLoc (TString loc _  :_) = loc
tokensLoc (TChar   loc _  :_) = loc
tokensLoc (TInt    loc _  :_) = loc
tokensLoc (TRat    loc _  :_) = loc
tokensLoc (TSpec   loc _  :_) = loc
tokensLoc (TError  loc _  :_) = loc
tokensLoc (TBrace  loc    :_) = loc
tokensLoc (TIndent loc    :_) = loc
tokensLoc (TPragma loc _  :_) = loc
tokensLoc (TEnd    loc    :_) = loc
tokensLoc _                   = mkLocEOF

readBase :: Integer -> String -> Integer
readBase b = foldl (\ r c -> r * b + toInteger (digitToInt c)) 0

readInt :: String -> Int
readInt = fromInteger . readBase 10

-- XXX This is a pretty hacky recognition of pragmas.
pragma :: SLoc -> [Char] -> [Token]
pragma loc cs =
  let as = map toUpper $ takeWhile isAlpha $ dropWhile isSpace cs
      skip = skipNest loc 1 ('#':cs)
  in  case as of
        "SOURCE" -> TPragma loc as : skip
        _ -> skip

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
                                                     (m:ms,_:_) | m/=0 -> (TEnd (tokensLoc ts),  LS $ layoutLS  ts     ms )
                                                     _ ->     (TError l "syntax error",  LS $ layoutLS  []     [] ) where l = tokensLoc ts
-- The rest are the Next commands
layoutLS tts@(TIndent x       : ts) mms@(m : ms) _ | n == m = (TSpec (tokensLoc ts) ';', LS $ layoutLS  ts    mms )
                                                   | n <  m = (TSpec (tokensLoc ts) '>', LS $ layoutLS tts     ms ) where {n = getCol x}
layoutLS     (TIndent _       : ts)          ms  _          =                                 layoutLS  ts     ms  Next
layoutLS     (TBrace x        : ts) mms@(m :  _) _ | n > m  = (TSpec (tokensLoc ts) '<', LS $ layoutLS  ts (n:mms)) where {n = getCol x}
layoutLS     (TBrace x        : ts)          []  _ | n > 0  = (TSpec (tokensLoc ts) '<', LS $ layoutLS  ts     [n]) where {n = getCol x}
layoutLS     (TBrace x        : ts)          ms  _          = (TSpec (tokensLoc ts) '<', LS $ layoutLS  (TSpec (tokensLoc ts) '>' : TIndent x : ts) ms)
layoutLS     (t@(TSpec _ '}') : ts)     (0 : ms) _          = (                       t, LS $ layoutLS  ts     ms )
layoutLS     (  (TSpec l '}') :  _)           _  _          = (TError l "layout error }",LS $ layoutLS  []     [] )
layoutLS     (t@(TSpec _ '{') : ts)          ms  _          = (                       t, LS $ layoutLS  ts  (0:ms))
layoutLS     ts@(t@(TEnd _)   :  _)          []  _          = (                       t, LS $ layoutLS  ts     [] )  -- repeat the TEnd token
layoutLS     ts@(TEnd l       :  _)     (_ : ms) _          = (TSpec l '>'             , LS $ layoutLS  ts     ms )  -- insert '>' and try again
layoutLS     (t               : ts)          ms  _          = (                       t, LS $ layoutLS  ts     ms )
layoutLS     []                               _  _          = error "layoutLS"

instance TokenMachine LexState Token where
  tmNextToken (LS f) = f Next
  tmRawTokens (LS f) =
    case f Raw of
      (TRaw ts, _) -> ts
      _            -> undefined

-- Used for Note 5.
popLayout :: LexState -> LexState
popLayout (LS f) = snd (f Pop)

-- Insert TBrace if no 'module'/'{'
lexStart :: [Token] -> [Token]
lexStart ts =
  case skip ts of
    TIdent _ [] "module" : _ -> ts
    TSpec _ '{'          : _ -> ts
    rs                       -> TBrace (tokensLoc ts) : rs
  where skip (TIndent _ : rs) = rs
        skip rs = rs

lexTopLS :: FilePath -> String -> LexState
lexTopLS f s = LS $ layoutLS (lexStart $ lex (SLoc f 1 1) s) []
  -- error $ show $ map showToken $ lex (SLoc f 1 1) s

-----------

-- Convert string in scientific notation to a rational number.
readRational :: String -> Rational
readRational "" = undefined
readRational acs@(sgn:as) | sgn == '-' = negate $ rat1 as
                          | otherwise  =          rat1 acs
  where
    rat1 s1 =
      case span isDigit s1 of
        (ds1, cr1) | ('.':r1) <- cr1                   -> rat2 f1 r1
                   | (c:r1)   <- cr1, toLower c == 'e' -> rat3 f1 r1
                   | otherwise                         -> f1
          where f1 = toRational (readBase 10 ds1)

    rat2 f1 s2 =
      case span isDigit s2 of
        (ds2, cr2) | (c:r2) <- cr2, toLower c == 'e' -> rat3 f2 r2
                   | otherwise                       -> f2
          where f2 = f1 + toRational (readBase 10 ds2) * 10 ^^ (negate $ length ds2)

    rat3 f2 ('+':s) = f2 * expo s
    rat3 f2 ('-':s) = f2 / expo s
    rat3 f2      s  = f2 * expo s

    expo s = 10 ^ (readBase 10 s)
