-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Translate(
  translate, translateAndRun
  ) where
import Prelude
import Data.Maybe
import qualified MicroHs.IdentMap as M
import System.Environment
import Unsafe.Coerce
import GHC.Types
import Compat
import PrimTable

import MicroHs.Desugar(LDef, encodeInteger)
import MicroHs.Expr
import MicroHs.Exp
import MicroHs.Ident

translateAndRun :: (Ident, [LDef]) -> IO ()
--translateAndRun :: (Ident, [(Ident, Exp)]) -> IO ()
translateAndRun defs = do
  -- Drop all argument up to '--'
  args <- getArgs
  let prog = unsafeCoerce $ translate defs
  withDropArgs (length (takeWhile (/= "--") args) + 1)
    prog

translate :: (Ident, [LDef]) -> Any
--translate :: (Ident, [(Ident, Exp)]) -> Any
translate (mainName, ds) =
  let
    look m n = fromMaybe (error $ "translate: not found " ++ showIdent n) $ M.lookup n m
    mp = M.fromList [(n, trans (look mp) d) | (n, d) <- ds ]
  in look mp mainName

trans :: (Ident -> Any) -> Exp -> Any
trans r ae =
  case ae of
    Var n -> r n
    App f a -> unsafeCoerce (trans r f) (trans r a)
    Lit (LInt i) -> unsafeCoerce i
    Lit (LDouble i) -> unsafeCoerce i
    Lit (LStr s) -> trans r (encodeString s)
    Lit (LPrim p) -> fromMaybe (error $ "trans: no primop " ++ p) $ lookup p primTable
    Lit (LInteger i) -> trans r (encodeInteger i)
    Lit (LForImp s) -> trans r (App (Lit (LPrim "dynsym")) (Lit (LStr s)))
    _ -> error $ "trans: impossible: " ++ show ae

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
  ("U", primitive "U"),
  ("Y", primitive "Y"),
  ("B'", primitive "B'"),
  ("Z", primitive "Z"),
  ("R", primitive "R"),
  ("+", primitive "+"),
  ("-", primitive "-"),
  ("*", primitive "*"),
  ("quot", primitive "quot"),
  ("rem", primitive "rem"),
  ("uquot", primitive "uquot"),
  ("urem", primitive "urem"),
  ("neg", primitive "neg"),
  ("and", primitive "and"),
  ("or", primitive "or"),
  ("xor", primitive "xor"),
  ("inv", primitive "inv"),
  ("shl", primitive "shl"),
  ("shr", primitive "shr"),
  ("ashr", primitive "ashr"),
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
  ("fneg", primitive "fneg"),
  ("fshow", primitive "fshow"),
  ("fread", primitive "fread"),
  ("itof", primitive "itof"),
  ("seq", primitive "seq"),
  ("error", primitive "error"),
  ("equal", primitive "equal"),
  ("compare", primitive "compare"),
  ("rnf", primitive "rnf"),
  ("noMatch", primitive "noMatch"),
  ("noDefault", primitive "noDefault"),
  ("IO.>>=", primitive "IO.>>="),
  ("IO.>>", primitive "IO.>>"),
  ("IO.return", primitive "IO.return"),
  ("IO.print", primitive "IO.print"),
  ("IO.serialize", primitive "IO.serialize"),
  ("IO.deserialize", primitive "IO.deserialize"),
  ("IO.stdin", primitive "IO.stdin"),
  ("IO.stdout", primitive "IO.stdout"),
  ("IO.stderr", primitive "IO.stderr"),
  ("IO.getArgs", primitive "IO.getArgs"),
  ("IO.dropArgs", primitive "IO.dropArgs"),
  ("IO.performIO", primitive "IO.performIO"),
  ("IO.catch", primitive "IO.catch"),
  ("dynsym", primitive "dynsym"),
  ("newCAStringLen", primitive "newCAStringLen"),
  ("peekCAString", primitive "peekCAString"),
  ("peekCAStringLen", primitive "peekCAStringLen"),
  ("toInt", primitive "toInt"),
  ("toPtr", primitive "toPtr"),
  ("toDbl", primitive "toDbl")
  ]
