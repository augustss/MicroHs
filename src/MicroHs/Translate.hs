-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-imports #-}
module MicroHs.Translate(
  translate,
  TranslateMap,
  translateMap,
  translateWithMap,
  translateAndRun
  ) where
import qualified Prelude(); import MHSPrelude
import Data.ByteString.Internal(ByteString)
import Data.Maybe
import Data.String
import Unsafe.Coerce
import PrimTable

import MicroHs.Desugar(LDef, encodeInteger)
import MicroHs.Expr
import MicroHs.Exp
import MicroHs.ExpPrint(encodeString)
import MicroHs.Ident
import qualified MicroHs.IdentMap as M

translateAndRun :: ([LDef], Exp) -> IO ()
translateAndRun defs = do
  let prog = unsafeCoerce (translate defs)
  prog

type TranslateMap = M.Map AnyType

trLookup :: TranslateMap -> Ident -> AnyType
trLookup mp n = fromMaybe (errorMessage (getSLoc n) $ "translate: not found " ++ showIdent n) $ M.lookup n mp

translateMap :: [LDef] -> TranslateMap
translateMap ds =
  let mp = M.fromList [(n, translateExp mp d) | (n, d) <- ds ]
  in  mp

translateExp :: TranslateMap -> Exp -> AnyType
translateExp mp e = trans (trLookup mp) e

translateWithMap :: TranslateMap -> ([LDef], Exp) -> AnyType
translateWithMap mp (ds, e) =
  let mp' = foldr (\ (i, d) -> M.insert i (translateExp mp' d)) mp ds
  in  translateExp mp' e

translate :: ([LDef], Exp) -> AnyType
translate (ds, e) = translateExp (translateMap ds) e

trans :: (Ident -> AnyType) -> Exp -> AnyType
trans r ae =
  case ae of
    Var n -> r n
    App f a -> unsafeCoerce (trans r f) (trans r a)
    Lit (LInt i) -> unsafeCoerce i
    Lit (LInt64 i) -> unsafeCoerce i
    Lit (LDouble i) -> unsafeCoerce i
    Lit (LFloat i) -> unsafeCoerce i
    Lit (LStr s) -> trans r (encodeString s)
    Lit (LBStr s) -> unsafeCoerce (Data.String.fromString s :: ByteString)
    Lit (LPrim p) -> fromMaybe (error $ "trans: no primop " ++ show p) $ lookup p primTable
    Lit (LInteger i) -> trans r (encodeInteger i)
    Lit (LForImp _ s _) -> trans r (App (Lit (LPrim "dynsym")) (Lit (LStr s)))
    _ -> error $ "trans: impossible: " ++ show ae

-- Use linear search in this table.
-- 99% of the hits are among the combinators.
primTable :: [(String, AnyType)]
primTable = [
  ("B", _primitive "B"),
  ("O", _primitive "O"),
  ("K", _primitive "K"),
  ("C'", _primitive "C'"),
  ("C", _primitive "C"),
  ("A", _primitive "A"),
  ("S'", _primitive "S'"),
  ("P", _primitive "P"),
  ("I", _primitive "I"),
  ("S", _primitive "S"),
  ("U", _primitive "U"),
  ("Y", _primitive "Y"),
  ("B'", _primitive "B'"),
  ("Z", _primitive "Z"),
--  ("J", _primitive "J"),
  ("R", _primitive "R"),
  ("K2", _primitive "K2"),
  ("K3", _primitive "K3"),
  ("K4", _primitive "K4"),
  ("C'B", _primitive "C'B"),
  ("+", _primitive "+"),
  ("-", _primitive "-"),
  ("*", _primitive "*"),
  ("quot", _primitive "quot"),
  ("rem", _primitive "rem"),
  ("uquot", _primitive "uquot"),
  ("urem", _primitive "urem"),
  ("neg", _primitive "neg"),
  ("and", _primitive "and"),
  ("or", _primitive "or"),
  ("xor", _primitive "xor"),
  ("inv", _primitive "inv"),
  ("shl", _primitive "shl"),
  ("shr", _primitive "shr"),
  ("ashr", _primitive "ashr"),
  ("subtract", _primitive "subtract"),
  ("popcount", _primitive "popcount"),
  ("clz", _primitive "clz"),
  ("ctz", _primitive "ctz"),
  ("==", _primitive "=="),
  ("/=", _primitive "/="),
  ("<", _primitive "<"),
  ("<=", _primitive "<="),
  (">", _primitive ">"),
  (">=", _primitive ">="),
  ("u<", _primitive "u<"),
  ("u<=", _primitive "u<="),
  ("u>", _primitive "u>"),
  ("u>=", _primitive "u>="),
  ("ord", _primitive "ord"),
  ("chr", _primitive "chr"),
  ("f+", _primitive "f+"),
  ("f-", _primitive "f-"),
  ("f*", _primitive "f*"),
  ("f/", _primitive "f/"),
  ("f==", _primitive "f=="),
  ("f/=", _primitive "f/="),
  ("f<", _primitive "f<"),
  ("f<=", _primitive "f<="),
  ("f>", _primitive "f>"),
  ("f>=", _primitive "f>="),
  ("fneg", _primitive "fneg"),
  ("itof", _primitive "itof"),
  ("Itof", _primitive "Itof"),
  ("d+", _primitive "d+"),
  ("d-", _primitive "d-"),
  ("d*", _primitive "d*"),
  ("d/", _primitive "d/"),
  ("d==", _primitive "d=="),
  ("d/=", _primitive "d/="),
  ("d<", _primitive "d<"),
  ("d<=", _primitive "d<="),
  ("d>", _primitive "d>"),
  ("d>=", _primitive "d>="),
  ("dneg", _primitive "dneg"),
  ("itod", _primitive "itod"),
  ("Itod", _primitive "Itod"),
  ("seq", _primitive "seq"),
  ("icmp", _primitive "icmp"),
  ("ucmp", _primitive "ucmp"),
  ("rnf", _primitive "rnf"),
  ("IO.>>=", _primitive "IO.>>="),
  ("IO.>>", _primitive "IO.>>"),
  ("IO.return", _primitive "IO.return"),
  ("IO.print", _primitive "IO.print"),
  ("IO.serialize", _primitive "IO.serialize"),
  ("IO.deserialize", _primitive "IO.deserialize"),
  ("IO.stdin", _primitive "IO.stdin"),
  ("IO.stdout", _primitive "IO.stdout"),
  ("IO.stderr", _primitive "IO.stderr"),
  ("IO.getArgRef", _primitive "IO.getArgRef"),
  ("IO.performIO", _primitive "IO.performIO"),
  ("IO.gc", _primitive "IO.gc"),
  ("IO.stats", _primitive "IO.stats"),
  ("raise", _primitive "raise"),
  ("catch", _primitive "catch"),
  ("dynsym", _primitive "dynsym"),
  ("newCAStringLen", _primitive "newCAStringLen"),
  ("packCString", _primitive "packCString"),
  ("packCStringLen", _primitive "packCStringLen"),
  ("toInt", _primitive "toInt"),
  ("toPtr", _primitive "toPtr"),
  ("toDbl", _primitive "toDbl"),
  ("toFlt", _primitive "toFlt"),
  ("fromDbl", _primitive "fromDbl"),
  ("fromFlt", _primitive "fromFlt"),
  ("toFunPtr", _primitive "toFunPtr"),
  ("A.alloc", _primitive "A.alloc"),
  ("A.copy", _primitive "A.copy"),
  ("A.size", _primitive "A.size"),
  ("A.read", _primitive "A.read"),
  ("A.write", _primitive "A.write"),
  ("A.trunc", _primitive "A.trunc"),
  ("A.==", _primitive "A.=="),
  ("bs++", _primitive "bs++"),
  ("bs++.", _primitive "bs++."),
  ("bs==", _primitive "bs=="),
  ("bs/=", _primitive "bs/="),
  ("bs<", _primitive "bs<"),
  ("bs<=", _primitive "bs<="),
  ("bs>", _primitive "bs>"),
  ("bs>=", _primitive "bs>="),
  ("bscmp", _primitive "bscmp"),
  ("bspack", _primitive "bspack"),
  ("bsunpack", _primitive "bsunpack"),
  ("bsreplicate", _primitive "bsreplicate"),
  ("bslength", _primitive "bslength"),
  ("bssubstr", _primitive "bssubstr"),
  ("bsindex", _primitive "bsindex"),
  ("bswrite", _primitive "bswrite"),
  ("fromUTF8", _primitive "fromUTF8"),
  ("toUTF8", _primitive "toUTF8"),
  ("headUTF8", _primitive "headUTF8"),
  ("tailUTF8", _primitive "tailUTF8"),
  ("fp+", _primitive "fp+"),
  ("fp2p", _primitive "fp2p"),
  ("fpnew", _primitive "fpnew"),
  ("fpfin", _primitive "fpfin"),
  ("fp2bs", _primitive "fp2bs"),
  ("bs2fp", _primitive "bs2fp"),
  ("IO.fork", _primitive "IO.fork"),
  ("IO.thid", _primitive "IO.thid"),
  ("thnum", _primitive "thnum"),
  ("IO.throwto", _primitive "IO.throwto"),
  ("IO.yield", _primitive "IO.yield"),
  ("IO.newmvar", _primitive "IO.newmvar"),
  ("IO.takemvar", _primitive "IO.takemvar"),
  ("IO.putmvar", _primitive "IO.putmvar"),
  ("IO.readmvar", _primitive "IO.readmvar"),
  ("IO.trytakemvar", _primitive "IO.trytakemvar"),
  ("IO.tryputmvar", _primitive "IO.tryputmvar"),
  ("IO.tryreadmvar", _primitive "IO.tryreadmvar"),
  ("IO.threaddelay", _primitive "IO.threaddelay"),
  ("IO.threadstatus", _primitive "IO.threadstatus"),
  ("isint", _primitive "isint"),
  ("SPnew", _primitive "SPnew"),
  ("SPderef", _primitive "SPderef"),
  ("SPfree", _primitive "SPfree"),

  ("I+", _primitive "I+"),
  ("I-", _primitive "I-"),
  ("I*", _primitive "I*"),
  ("Iquot", _primitive "Iquot"),
  ("Irem", _primitive "Irem"),
  ("Iuquot", _primitive "Iuquot"),
  ("Iurem", _primitive "Iurem"),
  ("Ineg", _primitive "Ineg"),
  ("Iand", _primitive "Iand"),
  ("Ior", _primitive "Ior"),
  ("Ixor", _primitive "Ixor"),
  ("Ishl", _primitive "Ishl"),
  ("Ishr", _primitive "Ishr"),
  ("Iashr", _primitive "Iashr"),
  ("Iinv", _primitive "Iinv"),
  ("Isubtract", _primitive "Isubtract"),
  ("Ipopcount", _primitive "Ipopcount"),
  ("Iclz", _primitive "Iclz"),
  ("Ictz", _primitive "Ictz"),
  ("I==", _primitive "I=="),
  ("I/=", _primitive "I/="),
  ("I<", _primitive "I<"),
  ("I<=", _primitive "I<="),
  ("I>", _primitive "I>"),
  ("I>=", _primitive "I>="),
  ("Iicmp", _primitive "Iicmp"),
  ("Iu<", _primitive "Iu<"),
  ("Iu<=", _primitive "Iu<="),
  ("Iu>", _primitive "Iu>"),
  ("Iu>=", _primitive "Iu>="),
  ("Iucmp", _primitive "Iucmp"),
  ("itoI", _primitive "itoI"),
  ("Itoi", _primitive "Itoi"),
  ("utoU", _primitive "utoU"),
  ("Utou", _primitive "Utou"),
  ("Wknew", _primitive "Wknew"),
  ("Wknewfin", _primitive "Wknewfin"),
  ("Wkderef", _primitive "Wkderef"),
  ("Wkfinal", _primitive "Wkfinal"),
  ("IO.getmaskingstate", _primitive "IO.getmaskingstate"),
  ("IO.setmaskingstate", _primitive "IO.setmaskingstate")
  ]
