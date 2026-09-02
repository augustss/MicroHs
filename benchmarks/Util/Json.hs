-- A minimal JSON value type and pretty-printer. We only need to emit JSON, never
-- parse it.
module Util.Json
  ( JSON(..)
  , jInt
  , jDouble
  , jString
  , jMaybe
  , renderJSON
  ) where
import Data.Char(ord)
import Numeric(showHex)

data JSON
  = JNull
  | JBool Bool
  | JInt Integer
  | JDouble Double
  | JString String
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Show, Eq)

jInt :: Integral a => a -> JSON
jInt = JInt . toInteger

-- JSON has no representation for Infinity/NaN; the RTS stats print "inf"
-- for a rate when the run was too short to divide by (see RunStats.hs), so
-- that case becomes JSON null rather than an invalid number literal.
jDouble :: Double -> JSON
jDouble d = if isInfinite d || isNaN d then JNull else JDouble d

jString :: String -> JSON
jString = JString

jMaybe :: (a -> JSON) -> Maybe a -> JSON
jMaybe = maybe JNull

--------------------------------------------------------------------------

renderJSON :: JSON -> String
renderJSON v = render 0 v ""

-- Each render function takes an indent level and a continuation string to
-- append (a difference-list style, so this stays linear in output size
-- instead of repeatedly appending with (++)).
render :: Int -> JSON -> ShowS
render _ JNull        = showString "null"
render _ (JBool b)    = showString (if b then "true" else "false")
render _ (JInt n)     = shows n
render _ (JDouble d)  = shows d
render _ (JString s)  = renderString s
render ind (JArray xs) = renderBlock ind '[' ']' (map (render (ind + 1)) xs)
render ind (JObject kvs) =
  renderBlock ind '{' '}' [ renderString k . showString ": " . render (ind + 1) v | (k, v) <- kvs ]

renderBlock :: Int -> Char -> Char -> [ShowS] -> ShowS
renderBlock _ open close [] = showChar open . showChar close
renderBlock ind open close items =
  showChar open . showChar '\n'
  . foldr1 (\a b -> a . showString ",\n" . b) [ indent (ind + 1) . item | item <- items ]
  . showChar '\n' . indent ind . showChar close
  where indent n = showString (replicate (n * 2) ' ')

renderString :: String -> ShowS
renderString s = showChar '"' . foldr (\c k -> escape c . k) id s . showChar '"'
  where
    escape '"'  = showString "\\\""
    escape '\\' = showString "\\\\"
    escape '\n' = showString "\\n"
    escape '\t' = showString "\\t"
    escape '\r' = showString "\\r"
    escape c
      | ord c < 0x20 = showString "\\u" . showString (pad4 (showHex (ord c) ""))
      | otherwise    = showChar c
    pad4 h = replicate (4 - length h) '0' ++ h
