module String where
import Prelude
import List

showChar :: Char -> String
showChar c =
  case ord c == ord '\n' of
    False -> ['\'', c, '\'']
    True  -> "'\\n'"

showString :: String -> String
showString s =
  let
    loop arg =
      case arg of
        [] -> "\""
        c : cs ->
          case ord c == ord '\n' of
            False -> c : loop cs
            True  -> '\\' : 'n' : loop cs
  in '"' : loop s

-- XXX wrong for minInt
showInt :: Int -> String
showInt n =
  case n < 0 of
    False -> showUnsignedInt n
    True  -> '-' : showUnsignedInt (negate n)

showUnsignedInt :: Int -> String
showUnsignedInt n =
  let
    c = chr (ord '0' + rem n 10)
  in  case n < 10 of
        False -> showUnsignedInt (quot n 10) ++ [c]
        True  -> [c]

showBool :: Bool -> String
showBool arg =
  case arg of
    False -> "False"
    True  -> "True"

--showUnit :: () -> String
--showUnit arg =
--  case arg of
--    () -> "()"

showPair :: (a -> String) -> (b -> String) -> (a, b) -> String
showPair sa sb ab =
  case ab of
    (a, b) -> "(" ++ sa a ++ "," ++ sb b ++ ")"

showList :: (a -> String) -> [a] -> String
showList sa arg =
  let
    showRest as =
      case as of
        [] -> "]"
        x : xs -> "," ++ sa x ++ showRest xs
  in
    case arg of
      [] -> "[]"
      a : as -> "[" ++ sa a ++ showRest as

showMaybe :: (a -> String) -> Maybe a -> String
showMaybe fa arg =
  case arg of
    Nothing -> "Nothing"
    Just a  -> "(Just " ++ fa a ++ ")"

showEither :: (a -> String) -> (b -> String) -> Either a b -> String
showEither fa fb arg =
  case arg of
    Left  a -> "(Left " ++ fa a ++ ")"
    Right b -> "(Right " ++ fb b ++ ")"
