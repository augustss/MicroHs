-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Translate(
  translate, translateAndRun
  ) where
import Prelude
import Data.Maybe
import qualified MicroHs.IdentMap as M
import System.Environment
--Ximport GHC.Types
import Unsafe.Coerce
--Ximport Compat
--Wimport PrimTable

import MicroHs.Expr
import MicroHs.Exp
import MicroHs.Ident

--translateAndRun :: (Ident, [LDef]) -> IO ()
translateAndRun :: (Ident, [(Ident, Exp)]) -> IO ()
translateAndRun defs = do
  -- Drop all argument up to '--'
  args <- getArgs
  let prog = unsafeCoerce $ translate defs
  withDropArgs (length (takeWhile (not . eqString "--") args) + 1)
    prog

--translate :: (Ident, [LDef]) -> Any
translate :: (Ident, [(Ident, Exp)]) -> Any
translate (mainName, ds) =
  let
    look m n = fromMaybe (error $ "not found " ++ showIdent n) $ M.lookup n m
    mp = M.fromList [(n, trans (look mp) d) | (n, d) <- ds ]
  in look mp mainName

trans :: (Ident -> Any) -> Exp -> Any
trans r ae =
  case ae of
    Var n -> r n
    App f a -> unsafeCoerce (trans r f) (trans r a)
    Lit (LInt i) -> unsafeCoerce i
    Lit (LStr s) -> trans r (encodeString s)
    Lit (LPrim p) -> fromMaybe (error $ "primlookup: " ++ p) $ lookupBy eqString p primTable
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
  ("BK", primitive "BK"),
  ("+", primitive "+"),
  ("-", primitive "-"),
  ("*", primitive "*"),
  ("quot", primitive "quot"),
  ("rem", primitive "rem"),
  ("uquot", primitive "uquot"),
  ("urem", primitive "urem"),
  ("subtract", primitive "subtract"),
  ("==", primitive "=="),
  ("/=", primitive "/="),
  ("<", primitive "<"),
  ("<=", primitive "<="),
  (">", primitive ">"),
  (">=", primitive ">="),
  ("u<", primitive "u<"),
  ("u<=", primitive "u<="),
  ("u>", primitive "u>"),
  ("u>=", primitive "u>="),
  ("fadd", primitive "fadd"),
  ("fsub", primitive "fsub"),
  ("fmul", primitive "fmul"),
  ("fdiv", primitive "fdiv"),
  ("feq", primitive "feq"),
  ("fne", primitive "fne"),
  ("flt", primitive "flt"),
  ("fle", primitive "fle"),
  ("fgt", primitive "fgt"),
  ("fge", primitive "fge"),
  ("fshow", primitive "fshow"),
  ("fread", primitive "fread"),
  ("seq", primitive "seq"),
  ("error", primitive "error"),
  ("equal", primitive "equal"),
  ("compare", primitive "compare"),
  ("rnf", primitive "rnf"),
  ("IO.>>=", primitive "IO.>>="),
  ("IO.>>", primitive "IO.>>"),
  ("IO.return", primitive "IO.return"),
  ("IO.getChar", primitive "IO.getChar"),
  ("IO.getRaw", primitive "IO.getRaw"),
  ("IO.putChar", primitive "IO.putChar"),
  ("IO.serialize", primitive "IO.serialize"),
  ("IO.deserialize", primitive "IO.deserialize"),
  ("IO.open", primitive "IO.open"),
  ("IO.close", primitive "IO.close"),
  ("IO.flush", primitive "IO.flush"),
  ("IO.isNullHandle", primitive "IO.isNullHandle"),
  ("IO.stdin", primitive "IO.stdin"),
  ("IO.stdout", primitive "IO.stdout"),
  ("IO.stderr", primitive "IO.stderr"),
  ("IO.getArgs", primitive "IO.getArgs"),
  ("IO.dropArgs", primitive "IO.dropArgs"),
  ("IO.performIO", primitive "IO.performIO"),
  ("IO.getTimeMilli", primitive "IO.getTimeMilli"),
  ("IO.catch", primitive "IO.catch"),
  ("isInt", primitive "isInt"),
  ("isIO", primitive "isIO")
  ]
