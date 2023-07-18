-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Translate(module MicroHs.Translate) where
import Prelude
import Data.Maybe
import qualified MicroHs.Map as M
--Ximport GHC.Types
import Unsafe.Coerce
--Ximport Compat
--Ximport PrimTable
--Yimport PrimTable

import MicroHs.Desugar
import MicroHs.Parse
import MicroHs.Exp

translate :: (Ident, [LDef]) -> IO ()
translate mds =
  case mds of
    (mainName, ds) ->
      let
        look m n = fromMaybe (error $ "not found " ++ n) $ M.lookup eqString n m
        --Xmp :: M.Map Ident Any
        mp = M.fromList [(n, trans (look mp) d) | (n, d) <- ds ]
      in  unsafeCoerce $ look mp mainName

trans :: (Ident -> Any) -> Exp -> Any
trans r ae =
  case ae of
    Var n -> r n
    App f a -> unsafeCoerce (trans r f) (trans r a)
    Int i -> unsafeCoerce i
    Prim p -> fromMaybe (error "primlookup") $ lookupBy eqString p primTable
    _ -> error "trans: impossible"

primTable :: [(String, Any)]
primTable = [
  ("S", primitive "S"),
  ("K", primitive "K"),
  ("I", primitive "I"),
  ("C", primitive "C"),
  ("B", primitive "B"),
  ("T", primitive "T"),
  ("Y", primitive "Y"),
  ("S'", primitive "S'"),
  ("C'", primitive "C'"),
  ("B'", primitive "B'"),
  ("P", primitive "P"),
  ("O", primitive "O"),
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
