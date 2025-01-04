-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Translate(
  translate, translateAndRun
  ) where
import Prelude(); import MHSPrelude
import Data.Maybe
import qualified MicroHs.IdentMap as M
import Unsafe.Coerce
import PrimTable

import MicroHs.Desugar(LDef, encodeInteger)
import MicroHs.Expr
import MicroHs.Exp
import MicroHs.ExpPrint(encodeString)
import MicroHs.Ident

translateAndRun :: (Ident, [LDef]) -> IO ()
translateAndRun defs = do
  let prog = unsafeCoerce $ translate defs
  prog

translate :: (Ident, [LDef]) -> AnyType
translate (mainName, ds) =
  let
    look m n = fromMaybe (error $ "translate: not found " ++ showIdent n) $ M.lookup n m
    mp = M.fromList [(n, trans (look mp) d) | (n, d) <- ds ]
  in look mp mainName

trans :: (Ident -> AnyType) -> Exp -> AnyType
trans r ae =
  case ae of
    Var n -> r n
    App f a -> unsafeCoerce (trans r f) (trans r a)
    Lit (LInt i) -> unsafeCoerce i
    Lit (LDouble i) -> unsafeCoerce i
    Lit (LStr s) -> trans r (encodeString s)
    Lit (LPrim p) -> fromMaybe (error $ "trans: no primop " ++ show p) $ lookup p primTable
    Lit (LInteger i) -> trans r (encodeInteger i)
    Lit (LForImp s _) -> trans r (App (Lit (LPrim "dynsym")) (Lit (LStr s)))
    _ -> error $ "trans: impossible: " ++ show ae

-- Use linear search in this table.
-- 99% of the hits are among the combinators.
primTable :: [(String, AnyType)]
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
  ("K2", primitive "K2"),
  ("K3", primitive "K3"),
  ("K4", primitive "K4"),
  ("C'B", primitive "C'B"),
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
  ("ord", primitive "ord"),
  ("chr", primitive "chr"),
  ("f+", primitive "f+"),
  ("f-", primitive "f-"),
  ("f*", primitive "f*"),
  ("f/", primitive "f/"),
  ("f==", primitive "f=="),
  ("f/=", primitive "f/="),
  ("f<", primitive "f<"),
  ("f<=", primitive "f<="),
  ("f>", primitive "f>"),
  ("f>=", primitive "f>="),
  ("fneg", primitive "fneg"),
  ("fshow", primitive "fshow"),
  ("fread", primitive "fread"),
  ("itof", primitive "itof"),
  ("seq", primitive "seq"),
  ("sequal", primitive "sequal"),
  ("equal", primitive "equal"),
  ("scmp", primitive "scmp"),
  ("icmp", primitive "icmp"),
  ("rnf", primitive "rnf"),
  ("IO.>>=", primitive "IO.>>="),
  ("IO.>>", primitive "IO.>>"),
  ("IO.return", primitive "IO.return"),
  ("IO.print", primitive "IO.print"),
  ("IO.serialize", primitive "IO.serialize"),
  ("IO.deserialize", primitive "IO.deserialize"),
  ("IO.stdin", primitive "IO.stdin"),
  ("IO.stdout", primitive "IO.stdout"),
  ("IO.stderr", primitive "IO.stderr"),
  ("IO.getArgRef", primitive "IO.getArgRef"),
  ("IO.performIO", primitive "IO.performIO"),
  ("IO.gc", primitive "IO.gc"),
  ("raise", primitive "raise"),
  ("catch", primitive "catch"),
  ("dynsym", primitive "dynsym"),
  ("newCAStringLen", primitive "newCAStringLen"),
  ("peekCAString", primitive "peekCAString"),
  ("peekCAStringLen", primitive "peekCAStringLen"),
  ("toInt", primitive "toInt"),
  ("toPtr", primitive "toPtr"),
  ("toDbl", primitive "toDbl"),
  ("toFunPtr", primitive "toFunPtr"),
  ("A.alloc", primitive "A.alloc"),
  ("A.size", primitive "A.size"),
  ("A.read", primitive "A.read"),
  ("A.write", primitive "A.write"),
  ("A.==", primitive "A.=="),
  ("bs++", primitive "bs++"),
  ("bs++.", primitive "bs++."),
  ("bs+++", primitive "bs+++"),
  ("bs==", primitive "bs=="),
  ("bs/=", primitive "bs/="),
  ("bs<", primitive "bs<"),
  ("bs<=", primitive "bs<="),
  ("bs>", primitive "bs>"),
  ("bs>=", primitive "bs>="),
  ("bscmp", primitive "bscmp"),
  ("bspack", primitive "bspack"),
  ("bsunpack", primitive "bsunpack"),
  ("bslength", primitive "bslength"),
  ("bssubstr", primitive "bssubstr"),
  ("fromUTF8", primitive "fromUTF8"),
  ("toUTF8", primitive "toUTF8"),
  ("headUTF8", primitive "headUTF8"),
  ("tailUTF8", primitive "tailUTF8"),
  ("fp+", primitive "fp+"),
  ("fp2p", primitive "fp2p"),
  ("fpnew", primitive "fpnew"),
  ("fpfin", primitive "fpfin")
  ]
