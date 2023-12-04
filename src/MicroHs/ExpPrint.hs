module MicroHs.ExpPrint(toStringP, encodeString) where
import Prelude
import Data.Char(ord)
import MicroHs.EncodeData(encList)
import MicroHs.Exp
import MicroHs.Expr(Lit(..), showLit)
import MicroHs.Ident(showIdent)

-- Avoid quadratic concatenation by using difference lists,
-- turning concatenation into function composition.
toStringP :: Exp -> (String -> String)
toStringP ae =
  case ae of
    Var x   -> (showIdent x ++)
    Lit (LStr s) ->
      -- Encode very short string directly as combinators.
      if length s > 1 then
        (quoteString s ++)
      else
        toStringP (encodeString s)
    Lit (LInteger _) -> undefined
    Lit (LRat _) -> undefined
    Lit l   -> (showLit l ++)
    Lam x e -> (("(\\" ++ showIdent x ++ " ") ++) . toStringP e . (")" ++)
    App f a -> ("(" ++) . toStringP f . (" " ++) . toStringP a . (")" ++)

quoteString :: String -> String
quoteString s =
  let
    achar c =
      if c == '"' || c == '\\' || c < ' ' || c > '~' then
        '\\' : show (ord c) ++ ['&']
      else
        [c]
  in '"' : concatMap achar s ++ ['"']

encodeString :: String -> Exp
encodeString = encList . map (Lit . LInt . ord)

