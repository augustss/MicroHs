module MicroHs.Lex(
  Token(..), showToken,
  tokensLoc,
  LexState, lexTopLS,
  popLayout, lex,
  readInt,
  interpSkip,
  ) where
import qualified Prelude(); import MHSPrelude hiding(lex)
import Data.Char
import Data.List
import Data.Maybe (fromJust)
import MicroHs.Ident
import Text.ParserComb(TokenMachine(..))
import Text.PrettyPrint.HughesPJLiteClass(prettyShow)
--import Debug.Trace

data Token
  = TIdent  SLoc [String] String  -- identifier
  | TString SLoc String           -- String literal
  | TQual   SLoc [String]         -- qualified literal
  | TChar   SLoc Char             -- Char literal
  | TInt    SLoc Integer          -- Integer literal
  | TRat    SLoc Rational         -- Rational literal (i.e., decimal number)
  | TSpec   SLoc Char             -- one of ()[]{},`<>;
                                  -- for synthetic {} we use <>, also
                                  --  .  for record selection
                                  --  ~  for lazy
                                  --  !  for strict
                                  --  NOT YET  @  for type app
                                  --  L  for (#
                                  --  R  for #)
                                  --  I  interpolation start
                                  --  E  interpolation end
                                  --  $  interpolation expr start
                                  --  %  interpolation expr end
  | TError  SLoc String           -- lexical error
  | TBrace  SLoc                  -- {n} in the Haskell report
  | TIndent SLoc                  -- <n> in the Haskell report
  | TPragma SLoc String           -- a {-# PRAGMA #-}
  | TEnd    SLoc
  | TRaw [Token]
--  deriving (Show)
instance Show Token where
  show = showToken

showToken :: Token -> String
showToken (TIdent _ ss s) = intercalate "." (ss ++ [s])
showToken (TString _ s) = show s
showToken (TQual _ ss) = concatMap (++ ".") ss
showToken (TChar _ c) = show c
showToken (TInt _ i) = show i
showToken (TRat _ d) = show d
showToken (TSpec _ c) | c == '<' = "{ layout"
                      | c == '>' = "} layout"
                      | c == 'L' = "(#"
                      | c == 'R' = "#)"
                      | otherwise = ['S',c]
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

-- Columns are numbered from 1, so tabs stop are 1, 9, 17, ...
tabCol :: SLoc -> SLoc
tabCol (SLoc f l c) = SLoc f l (((c + 7) `quot` 8) * 8 + 1)

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
lex loc ('s':'"':'"':'"':cs) = lexLitStr loc (addCol loc 4) (mkInterp loc) isTrip   multiLine interpSkip cs
lex loc ('s':'"':        cs) = lexLitStr loc (addCol loc 2) (mkInterp loc) isDQuote id        interpSkip cs
lex loc (d:cs) | isLower_ d =
  case spanIdent cs of
    (ds, rs) -> tIdent loc [] (d:ds) (lex (addCol loc $ 1 + length ds) rs)
lex loc cs@(d:_) | isUpper d = upperIdent loc loc [] cs
lex loc ('0':x:cs)
  | toLower x == 'x' = lexNumBasePrefix x 16 isHexDigit loc cs
  | toLower x == 'o' = lexNumBasePrefix x  8 isOctDigit loc cs
  | toLower x == 'b' = lexNumBasePrefix x  2 isBinDigit loc cs
  where isBinDigit c = c == '0' || c == '1'
lex loc cs@(d:_) | isDigit d =
  case readNumDec cs of
    (Left n,  len, rs) -> TInt loc n : lexSkipHash (addCol loc len) rs
    (Right q, len, rs) -> TRat loc q : lexSkipHash (addCol loc len) rs
lex loc ('.':cs@(d:_)) | isLower_ d =
  TSpec loc '.' : lex (addCol loc 1) cs
lex loc ('(':dcs@(d:cs)) | d == '#'  = TSpec loc 'L' : lex (addCol loc 2) cs
                         | otherwise = TSpec loc '(' : lex (addCol loc 1) dcs
lex loc ('#':')':cs) = TSpec loc 'R' : lex (addCol loc 2) cs
-- Recognize #line 123 "file/name.hs"
lex loc ('#':xcs) | (SLoc _ _ 1) <- loc, Just cs <- stripPrefix "line " xcs =
  case span (/= '\n') cs of
    (line, rs) ->        -- rs will contain the '\n', so subtract 1 below
      let ws = words line
          file = tail $ init $ ws!!1   -- strip the initial and final '"'
          loc' = SLoc file (readInt (ws!!0) - 1) 1
      in  lex loc' rs
                  | (SLoc _ 1 1) <- loc, take 1 xcs == "!" =
  -- It's a shebang (#!), ignore the rest of the line
  skipLine loc xcs
lex loc ('!':' ':cs) =  -- ! followed by a space is always an operator
  TIdent loc [] "!" : lex (addCol loc 2) cs
lex loc (c:cs@(d:_)) | isSpecSing c && not (isOperChar d) = -- handle reserved
  TSpec loc c :
    let ts = lex (addCol loc 1) cs
    in  if c == '\\' then tLam ts else ts
lex loc (d:cs) | isOperChar d =
  case span isOperChar cs of
    (ds, rs) -> TIdent loc [] (d:ds) : lex (addCol loc $ 1 + length ds) rs
lex loc (d:cs) | isSpec d =
  TSpec loc d : lex (addCol loc 1) cs
lex loc ('"':'"':'"':cs) = lexLitStr loc (addCol loc 3) (\ _ s -> [TString loc s]) isTrip   multiLine (\ _ _ -> Nothing) cs
lex loc ('"':cs)         = lexLitStr loc (addCol loc 1) (\ _ s -> [TString loc s]) isDQuote id        (\ _ _ -> Nothing) cs
lex loc ('\'':cs)        = lexLitStr loc (addCol loc 1) tchar                      isSQuote id        (\ _ _ -> Nothing) cs
  where isSQuote ('\'':_) = Just 1
        isSQuote _ = Nothing
        tchar _ [c] = [TChar loc c]
        tchar _ _   = [TError loc "Illegal Char literal"]

lex loc (d:_) = [TError loc $ "Unrecognized input: " ++ show d]
lex loc [] = [TEnd loc]

isTrip, isDQuote :: String -> Maybe Int
isTrip ('"':'"':'"':_) = Just 3
isTrip _ = Nothing
isDQuote ('"':_) = Just 1
isDQuote _ = Nothing

nested :: SLoc -> [Char] -> [Token]
nested loc ('#':cs) = pragma loc cs
nested loc cs = skipNest loc 1 cs

-- lex a number of the form '0':x:cs
lexNumBasePrefix :: Char -> Integer -> (Char -> Bool) -> SLoc -> String -> [Token]
lexNumBasePrefix x base isDig loc cs =
  case readIntBase base isDig cs of
    Just (n, len, rs) -> TInt loc n : lexSkipHash (addCol loc $ len + 2) rs
    Nothing           -> TInt loc 0 : lexSkipHash (addCol loc 1) (x : cs)

-- Used to skip # after numbers
lexSkipHash :: SLoc -> String -> [Token]
lexSkipHash loc ('#':cs) = lexSkipHash (addCol loc 1) cs
lexSkipHash loc cs = lex loc cs

readIntBase :: Integer -> (Char -> Bool) -> String -> Maybe (Integer, Int, String)
readIntBase base isDig ds =
    let (n, len, rest) = goDig 0 0 ds
    in if len > 0 then Just (n, len, rest) else Nothing
  where
    goSep acc lastLen lastRest len ('_' : cs) = goSep acc lastLen lastRest (len + 1) cs
    goSep acc _ _ len (d : cs) | isDig d = goDig (addDigit acc d) (len + 1) cs
    goSep acc lastLen lastRest _ _ = (acc, lastLen, lastRest)

    goDig acc len rest@('_' : cs) = goSep acc len rest (len + 1) cs
    goDig acc len (d : cs) | isDig d = goDig (addDigit acc d) (len + 1) cs
    goDig acc len rest = (acc, len, rest)

    addDigit x d = x * base + toInteger (digitToInt d)

readNumDec :: String -> (Either Integer Rational, Int, String)
readNumDec cs =
  case readIntDec cs of
    Just (n, nLen, rest) ->
      case rest of
        '.' : rs@(d : _) | isDigit d ->
          case readIntDec rs of
            Just (m, mLen, rest') ->
              let q = toRational n + toRational m * 10 ^^ negate (length $ filter isDigit $ take mLen rs)
              in case expo rest' of
                Just (e, eLen, rest'') -> (Right $ q * 10 ^^ e, nLen + 1 + mLen + eLen, rest'')
                Nothing -> (Right q, nLen + 1 + mLen, rest')
            Nothing -> (Left n, nLen, rest) -- this can't happen
        _ ->
          case expo rest of
            Just (e, eLen, rest') -> (Right $ toRational n * 10 ^^ e, nLen + eLen, rest')
            Nothing -> (Left n, nLen, rest)
    Nothing -> error "impossible: first char is a digit"
  where
    readIntDec = readIntBase 10 isDigit

    -- try to read an exponent
    expo :: String -> Maybe (Integer, Int, String)
    expo = go 0

    go len ('_' : xs) = go (len + 1) xs
    go len (e:'-':xs@(d:_)) | toLower e == 'e' && isDigit d =
      let (n, len', rest) = fromJust $ readIntDec xs
      in Just (-n, len + 2 + len', rest)
    go len (e:'+':xs@(d:_)) | toLower e == 'e' && isDigit d =
      let (n, len', rest) = fromJust $ readIntDec xs
      in Just (n, len + 2 + len', rest)
    go len (e:    xs@(d:_)) | toLower e == 'e' && isDigit d =
      let (n, len', rest) = fromJust $ readIntDec xs
      in Just (n, len + 1 + len', rest)
    go _ _ = Nothing

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

lexLitStr :: SLoc -> SLoc -> ([[Token]] -> String -> [Token]) -> (String -> Maybe Int) ->
             (String -> String) -> (SLoc -> String -> Maybe ([Token], String, SLoc)) -> String -> [Token]
lexLitStr oloc loc mk end post interp acs = loop loc [] [] acs
  where
        loop :: SLoc -> String -> [[Token]] -> String -> [Token]
        loop l rs tss cs | Just k <- end cs   = mk (reverse tss) (decodeEscs $ post $ reverse rs) ++ lex (addCol l k) (drop k cs)
                         | Just (ts, cs', l') <- interp l cs = loop l' (chMark : rs) (ts : tss) cs'
        loop l rs tss ('\\':c:cs) | isSpace c = remGap l rs tss cs
        loop l rs tss ('\\':'^':'\\':cs)      = loop (addCol l 3) ('\\':'^':'\\':rs) tss cs  -- special hack for unescaped \
        loop l rs tss ('\\':cs)               = loop' (addCol l 1) ('\\':rs) tss cs
        loop l rs tss       cs                = loop' l rs tss cs

        loop' :: SLoc -> String -> [[Token]] -> String -> [Token]
        loop' l rs tss ('\n' :cs) = loop  (incrLine l) ( '\n':rs) tss cs
        loop' l rs tss ('\t' :cs) = loop  (tabCol   l) ( '\t':rs) tss cs
        loop' l rs tss ('\r' :cs) = loop            l         rs  tss cs
        loop' l rs tss     (c:cs) = loop  (addCol l 1) (    c:rs) tss cs
        loop' _ _  _           [] = [TError oloc "unterminated Char/String literal"]

        remGap :: SLoc -> String -> [[Token]] -> String -> [Token]
        remGap l rs tss ('\\':cs) = loop   (addCol l 1)       rs  tss cs
        remGap l rs tss ('\n':cs) = remGap (incrLine l) ('\n':rs) tss cs
        remGap l rs tss ('\t':cs) = remGap (tabCol   l) ('\t':rs) tss cs
        remGap l rs tss ('\r':cs) = remGap           l        rs  tss cs
        remGap l rs tss (' ' :cs) = remGap (addCol l 1)       rs  tss cs
        remGap l _  _          _  = --errorMessage oloc "bad string gap"
                                    mhsError (prettyShow l ++ ": bad string gap")

chMark :: Char
chMark = '\xffff'     -- reserved, and will not occur in text

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
decodeEsc cs@(c:_) | isDigit c = conv 10 0 cs
decodeEsc (c1:c2:c3:cs) | Just c <- lookup [c1,c2,c3] ctlCodes = c : decodeEscs cs
decodeEsc (c1:c2:cs) | Just c <- lookup [c1,c2] ctlCodes = c : decodeEscs cs
decodeEsc (c  :cs) = c : decodeEscs cs
decodeEsc []       = mhsError "Bad \\ escape"

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
conv b r (c:ds) | isHexDigit c && n < b = conv b (r * b + n) ds
  where n = digitToInt c
conv _ r ds = chr r : decodeEscs ds

-- Multiline string literals
multiLine :: String -> String
multiLine =
  finalTrim          .     -- trim initial \n
  intercalate "\\n"  .     -- join with \n
  map removeAllWhite .     -- remove white-space only
  removeCommonPrefix .     -- remove common space prefix
  map tabToSpace     .     -- replace leading tabs with spaces
  lines                    -- split the string by newlines
  where
    tabToSpace = to 0
      where to n ('\t':cs) = replicate (8 - n `rem` 8) ' ' ++ to 0 cs
            to n (' ' :cs) = ' ' : to (n+1) cs
            to _ cs        = cs
    removeCommonPrefix :: [String] -> [String]
    removeCommonPrefix [] = []
    removeCommonPrefix (l:ls) = l : map (drop k) ls
      where k = foldl' pref 1000000 ls
            pref n [] = n
            pref n cs =
              case span isSpace cs of
                (_, []) -> n                 -- ignore white space only
                (w, _)  -> min n (length w)  -- find common prefix length
    removeAllWhite cs | all isSpace cs = ""
                      | otherwise      = cs
    -- The GHC manual is wrong.  Follow the implementation.
    finalTrim ('\\':'n':cs) = cs
    finalTrim cs            = cs

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

-- Called with current location, starting location, qualifiers so far (reverse),
-- and string to parse.
upperIdent :: SLoc -> SLoc -> [String] -> String -> [Token]
--upperIdent l c qs acs | trace (show (l, c, qs, acs)) False = undefined
upperIdent loc sloc qs acs =
  case span isIdentChar acs of
   (ds, rs) ->
    case rs of
                   -- qualified string, maybe with interpolation
      '.':cs@(d1:d2:_) | d1 == '"' || d1 == 's' && d2 == '"'    -- M."..." or M.s"..."
                                  -> TQual sloc (reverse (ds:qs)) : lex (addCol loc $ 1 + length ds) cs

      '.':cs@(d:_) -- either another module name or a qualified uppercase identifier
                   | isUpper d    -> upperIdent (addCol loc $ 1 + length ds) sloc (ds:qs) cs
                   -- qualified lower case identifier
                   | isLower_ d   -> ident (spanIdent cs)
                   -- qualified operator
                   | isOperChar d -> ident (span isOperChar cs)
                   -- could add qualified numbers here
         where
           ident (xs, ys) = tIdent sloc (reverse (ds:qs)) xs (lex (addCol loc $ 1 + length ds + length xs) ys)
      -- Identifier with trailing #
      '#':_ -> mk (ds ++ hs) rs' where (hs, rs') = span (== '#') rs
      _ -> mk ds rs
  where
     mk ds rs = TIdent sloc (reverse qs) ds : lex (addCol loc $ length ds) rs

spanIdent :: String -> (String, String)
spanIdent s = mk $ span isIdentChar s
  where mk (ds, '#':rs) = mk (ds ++ "#", rs)
        mk r = r

-- For LambdaCase
tLam :: [Token] -> [Token]
tLam (t@(TIdent _ [] "case") : ts) = t : tBrace ts
tLam ts = ts

tIdent :: SLoc -> [String] -> String -> [Token] -> [Token]
tIdent loc qs kw ats | elem kw ["let", "where", "do", "of", "mdo"]
                       || kw == "if" && isBar ats                -- For MultiWayIf
                                 = ti : tBrace ats
                     | otherwise = ti : ats
  where
    isBar (TSpec _ '|' : _) = True
    isBar (TIndent _ : TSpec _ '|' : _) = True
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
tokensLoc (TQual   loc _  :_) = loc
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
  let skip = skipNest loc 1 ('#':cs)
  in  case words cs of
        p : _ | map toUpper p == "SOURCE" -> TPragma loc p : skip
        -- hsc2hs generates LINE pragmas
        p : ln@(_:_) : fn : _ | map toUpper p == "LINE", all isDigit ln ->
          let f = tail (init fn)
              l = readInt ln - 1
          in  seq l $ skipNest (SLoc f l 1) 1 ('#':cs)
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
layoutLS     (t@(TIdent _ _ "do") :  -- for NondecreasingIndentation
              TBrace x        : ts) mms@(m :  _) _ | n >= m = (t                       , LS $ layoutLS  (TSpec (tokensLoc ts) '<' : ts) (n:mms)) where {n = getCol x}
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

-------

-- String interpolation proceeds in several steps.
--  * interpSkip is used to skip over ${expr} parts, and insert '\xffff' for each interpoland.
--  * do regular string processing of that string (gaps, escapes, multiline, etc)
--  * splice the interpolands back into the processed strings, breaking it at '\xffff'

-- Recognize ${ and find the corresponding }.
-- Return the (yokens for characters skipped, rest, new location)
-- Complicated because we want to keep all characters inside ${...},
-- but also correctly identifying the closing }, whilst tracking the location.
interpSkip :: SLoc -> String -> Maybe ([Token], String, SLoc)
interpSkip aloc ('$':'{':acs) =
--  trace ("interpSkip " ++ acs) $
  skip 0 "" aloc' acs
  where aloc' = addCol aloc 2
        skip :: Int -> String -> SLoc -> String -> Maybe ([Token], String, SLoc)
        skip 0 rs l ('}':cs)         = Just (recLex aloc' (reverse rs), cs, addCol l 1)
        skip n rs l ('{':'-':cs)     = skipn  n     ('-':'{':rs)           l 1 cs
        skip n rs l ('-':'-':cs)     = skipl  n     ('-':'-':rs)           l   cs
        skip n rs l ('{':cs)         = skip (n+1)       ('{':rs) (addCol l 1)  cs
        skip n rs l ('}':cs)         = skip (n-1)       ('}':rs) (addCol l 1)  cs
        skip n rs l ('"':'"':'"':cs) = skipss n ('"':'"':'"':rs) (addCol l 3)  cs
        skip n rs l ('"':cs)         = skips  n ('"'        :rs) (addCol l 1)  cs
        skip n rs l ('\'':cs)        = skipc  n ('\''       :rs) (addCol l 1)  cs
        skip n rs l ('\t':cs)        = skip   n ('\t'       :rs) (tabCol l)    cs
        skip n rs l ('\n':cs)        = skip   n ('\n'       :rs) (incrLine l)  cs
        skip n rs l ('\r':cs)        = skip   n              rs             l  cs
        skip n rs l (c:cs)           = skip   n (c          :rs) (addCol l 1)  cs
        skip _  _ _ []               = Nothing

        -- Skip """...""" string.  Keep all the characters, and keep track of position.
        skipss n rs l ('\\':c     :cs) = skipss n (c:'\\'     :rs) (addCol l 2) cs
        skipss n rs l ('"':'"':'"':cs) = skip   n ('"':'"':'"':rs) (addCol l 3) cs
        skipss n rs l (c          :cs) = skipss n (c          :rs) (addCol l 1) cs
        skipss n rs l []               = skip   n              rs            l  []

        -- Skip "..." string.  Keep all the characters, and keep track of position.
        skips n rs l ('"'   :cs) = skip  n ('"'   :rs) (addCol l 1) cs
        skips n rs l ('\\':c:cs) = skips n (c:'\\':rs) (addCol l 2) cs
        skips n rs l (     c:cs) = skips n (c     :rs) (addCol l 1) cs
        skips n rs l []          = skip  n         rs            l  []

        -- Skip '...' string.  Keep all the characters, and keep track of position.
        skipc n rs l ('\''  :cs) = skip  n ('\''  :rs) (addCol l 1) cs
        skipc n rs l ('\\':c:cs) = skipc n (c:'\\':rs) (addCol l 2) cs
        skipc n rs l (     c:cs) = skipc n (c     :rs) (addCol l 1) cs
        skipc n rs l []          = skip  n         rs            l  []

        -- Skip -- comment.  Keep all the characters, and keep track of position.
        skipl n rs l cs@('\n':_) = skip  n    rs            l  cs
        skipl n rs l (c:cs)      = skipl n (c:rs) (addCol l 1) cs
        skipl n rs l []          = skip  n    rs            l  []

        -- Skip {- -} comment.  Keep all the characters, and keep track of position.
        skipn :: Int -> String -> SLoc -> Int -> String -> Maybe ([Token], String, SLoc)
        skipn n rs l 0 cs           = skip  n          rs            l          cs
        skipn n rs l d ('{':'-':cs) = skipn n ('-':'{':rs) (addCol l 2) (d + 1) cs
        skipn n rs l d ('-':'}':cs) = skipn n ('}':'-':rs) (addCol l 2) (d - 1) cs
        skipn n rs l d ('\n':cs)    = skipn n ('\n'   :rs) (incrLine l)  d      cs
        skipn n rs l d ('\t':cs)    = skipn n ('\t'   :rs) (tabCol   l)  d      cs
        skipn n rs l d ('\r':cs)    = skipn n          rs            l   d      cs
        skipn n rs l d (c:cs)       = skipn n (c      :rs) (addCol l 1)  d      cs
        skipn n rs l _ []           = skip  n          rs            l          []

        -- lex an expression to be interpolated.  Add interpolation markers.
        recLex :: SLoc -> String -> [Token]
        recLex l s = [TSpec l '$'] ++ init ls ++ [TSpec ll '%']
          where ls = lex l s
                ll = tokensLoc [last ls]

interpSkip _ _ = Nothing

-- Splice in the interpolated tokens into the string
mkInterp :: SLoc -> [[Token]] -> String -> [Token]
mkInterp l atss as = TSpec l 'I' : splice atss [] as
  where splice _        rs []                   = mkS rs ++ [TSpec l 'E']
        splice (ts:tss) rs (c:cs) | c == chMark = mkS rs ++ ts ++ splice tss [] cs
        splice tss      rs (c:cs)               = splice tss (c:rs) cs

        mkS "" = []
        mkS rs = [TString l (reverse rs)]
