-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Translate(
  translate
  ) where
import Prelude
import Data.Maybe
import qualified MicroHs.StringMap as M
--Ximport GHC.Types
import Unsafe.Coerce
--Ximport Compat
--Ximport PrimTable
--Yimport PrimTable

import MicroHs.Desugar
import MicroHs.Parse
import MicroHs.Exp

translate :: (Ident, [LDef]) -> IO ()
translate (mainName, ds) =
  let
    look m n = fromMaybe (error $ "not found " ++ n) $ M.lookup n m
    --Xmp :: M.Map Any
    mp = M.fromList [(n, trans (look mp) d) | (n, d) <- ds ]
  in  unsafeCoerce $ look mp mainName

trans :: (Ident -> Any) -> Exp -> Any
trans r ae =
  case ae of
    Var n -> r n
    App f a -> unsafeCoerce (trans r f) (trans r a)
    Lit (LInt i) -> unsafeCoerce i
    Lit (LStr s) -> trans r (encodeString s)
    Lit (LPrim p) -> fromMaybe (error "primlookup") $ lookupBy eqString p primTable
    _ -> error "trans: impossible"

-- Use linear search in this table.
-- 99% of the hits are among the combinators.
primTable :: [(String, Any)]
primTable = [
  ("B", primitive "B"),
  ("O", primitive "O"),
  ("K", primitive "K"),
  ("C'", primitive "C'"),
  ("C", primitive "C"),
  ("A", primitive "A"),
  ("S'", primitive "S'"),
  ("P", primitive "P"),
  ("I", primitive "I"),
  ("S", primitive "S"),
  ("T", primitive "T"),
  ("Y", primitive "Y"),
  ("B'", primitive "B'"),
  ("+", primitive "+"),
  ("-", primitive "-"),
  ("*", primitive "*"),
  ("quot", primitive "quot"),
  ("rem", primitive "rem"),
  ("subtract", primitive "subtract"),
  ("==", primitive "=="),
  ("/=", primitive "/="),
  ("<", primitive "<"),
  ("<=", primitive "<="),
  (">", primitive ">"),
  (">=", primitive ">="),
  ("error", primitive "error"),
  ("IO.>>=", primitive "IO.>>="),
  ("IO.>>", primitive "IO.>>"),
  ("IO.return", primitive "IO.return"),
  ("IO.getChar", primitive "IO.getChar"),
  ("IO.putChar", primitive "IO.putChar"),
  ("IO.serialize", primitive "IO.serialize"),
  ("IO.deserialize", primitive "IO.deserialize"),
  ("IO.open", primitive "IO.open"),
  ("IO.close", primitive "IO.close"),
  ("IO.isNullHandle", primitive "IO.isNullHandle"),
  ("IO.stdin", primitive "IO.stdin"),
  ("IO.stdout", primitive "IO.stdout"),
  ("IO.stderr", primitive "IO.stderr"),
  ("IO.getArgs", primitive "IO.getArgs"),
  ("IO.performIO", primitive "IO.performIO")
  ]
