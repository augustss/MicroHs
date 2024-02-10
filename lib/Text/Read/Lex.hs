module Text.Read.Lex(lex, dropSpace) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Bool
import Data.Char
import Data.Eq
import Data.List
import Data.Ord

type ReadS a = String -> [(a, String)]

lex :: ReadS String
lex []       = [("","")]
lex ('\'':s) = [('\'':ch++"'", t) | (ch,'\'':t) <- lexLitChar s,
                                    ch /= "'" ]
lex ('"':s)  = [('"':str, t) | (str,t) <- lexString s]
  where
    lexString ('"':s) = [("\"",s)]
    lexString s = [(ch++str, u)
                  | (ch,t)  <- lexStrItem s,
                    (str,u) <- lexString t ]

    lexStrItem ('\\':'&':s) =  [("\\&",s)]
    lexStrItem ('\\':c:s) | isSpace c = [("\\&",t) |
                                         '\\':t <- [dropSpace s]]
    lexStrItem s = lexLitChar s

lex (c:s) | isSpace c  = lex s
          | isSingle c = [([c],s)]
          | isSym c    = [(c:sym,t)    | (sym,t) <- [span isSym s]]
          | isAlpha c  = [(c:nam,t)    | (nam,t) <- [span isIdChar s]]
          | isDigit c  = [(c:ds++fe,t) | (ds,s)  <- [span isDigit s],
                                         (fe,t)  <- lexFracExp s ]
          | otherwise  = []    -- bad character  
  where  
    isSingle c = c `elem` ",;()[]{}_`"
    isSym c    = c `elem` "!@#$%&?+./<=>?\\^|:-~"
    isIdChar c = isAlphaNum c || c `elem` "_'"

    lexFracExp ('.':c:cs) | isDigit c
      = [('.':ds++e,u) | (ds,t) <- lexDigits (c:cs),
         (e,u) <- lexExp t]
    lexFracExp s = lexExp s
 
    lexExp (e:s) | e `elem` "eE"
      = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
          (ds,u) <- lexDigits t] ++
        [(e:ds,t)   | (ds,t) <- lexDigits s]
    lexExp s = [("",s)]

lexDigits :: ReadS String
lexDigits s = [(cs, t) | (cs@(_:_), t) <- [span isDigit s]]

lexLitChar :: ReadS String
lexLitChar ('\\':s) = [ prefix '\\' c | c <- lexEsc s ]
  where
    lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c],s)]
    lexEsc ('^':c:s) | c >= '@' && c <= '_'    = [(['^',c],s)]
    lexEsc ('o':s)               = [prefix 'o' (span isOctDigit s)]
    lexEsc ('x':s)               = [prefix 'x' (span isHexDigit s)]
    lexEsc s@(d:_)   | isDigit d = [span isDigit s]
    lexEsc _                     = []

    prefix c (t,s) = (c:t, s)
lexLitChar (c:cs) = [([c], cs)]
lexLitChar [] = []

dropSpace :: String -> String
dropSpace [] = []
dropSpace ccs@(c:cs) | isSpace c = dropSpace cs
                     | True      = ccs
