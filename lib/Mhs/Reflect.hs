-- Copyright 2026 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Reflection: turn a value back into an expression.
-- The value is serialized to the combinator file format (see doc/COMB.md),
-- the combinator code is parsed, and then decompiled to lambda expressions
-- by reducing the combinators symbolically.  Constructors (C_k_n) and the
-- case combinators (D_a0_..._a{m-1} and E_k1_n1_..._km_nm) become constructors
-- and case expressions.  The number of fields of each arm comes from the
-- arities in the case combinator.
-- The definitions are named by their labels, or by their names when the
-- file has them (mhs -oFILE.combffi).
module Mhs.Reflect(
  Ident, Exp(..), Literal(..), Pat(..), Prog(..),
  reflect, reflectCombinators, hReflect,
  parseCombinators, decompile,
  ppProg, ppExp,
  ) where
import Control.Monad
import Data.ByteString.Char8(unpack, pack, ByteString, hGetContents)
import Data.Char(isAlphaNum, isDigit, isSpace, chr, ord)
import Data.Int(Int64)
import Data.List(isSuffixOf, stripPrefix)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Foreign.Ptr(Ptr)
import Mhs.Array(accumArray, (!))
import Mhs.Text.PrettyPrint.HughesPJLiteClass(Pretty(..), Doc, text, (<+>), sep, vcat, hsep, nest, hang,
                                           punctuate, maybeParens, render, appPrec)
import System.IO(Handle, hLookAhead, hGetChar)
import System.IO.Internal(BFILE, addTransducer)
import System.IO.Serialize(writeSerializedBS)

type Ident = String

data Exp
  = Var Ident
  | Lam Ident Exp
  | App Exp Exp
  | Lit Literal
  | Con Int Int                   -- constructor number k with n fields, C_k_n
  | Case Exp [(Pat, Exp)] (Maybe Exp)  -- scrutinee, arms, default
  | Let [(Ident, Exp)] Exp        -- recursive bindings
  | Array [Exp]
  deriving (Show)

data Pat = PCon Int Int [Ident]   -- C_k_n x1 ... xn
  deriving (Show)

data Literal
  = LInt Int
  | LInt64 Int64
  | LDouble Double
  | LFloat Float
  | LInteger Integer
  | LBytes ByteString             -- a ByteString
  | LString String                -- a String, from fromUTF8 applied to a ByteString
  | LPrim String                  -- a combinator or a primitive
  | LFFI String                   -- a foreign import
  | LFunPtr String                -- a C function pointer
  | LTick String                  -- a tick, for profiling
  deriving (Show)

-- Shared subexpressions are bound by a (recursive) let.
data Prog = Prog [(Ident, Exp)] Exp
  deriving (Show)

-- | Turn a value into an expression.
reflect :: a -> IO Prog
reflect a = decompile <$> reflectCombinators a

-- | Turn a value into its combinator code.
reflectCombinators :: a -> IO Prog
reflectCombinators a = parseCombinators . unpack <$> writeSerializedBS a

foreign import ccall "add_lzma_decompressor" c_add_lzma_decompressor :: Ptr BFILE -> IO (Ptr BFILE)

-- | Read a serialized value, e.g., a combinator file written by mhs or a
-- file written by writeSerialized, and turn it into an expression.
-- A .combffi file written by mhs has the names of the definitions,
-- and then they are used instead of the labels.
-- The file may be compressed (mhs -z, or writeSerializedCompressed).
-- Open the file with openBinaryFile, since it may contain bytes that
-- are not UTF-8.  The handle is closed.
hReflect :: Handle -> IO Prog
hReflect h = do
  c <- hLookAhead h
  h' <- if c == 'z' || c == 'q' then do        -- compressed?
          _ <- hGetChar h
          addTransducer c_add_lzma_decompressor h
        else
          return h
  decompile . parseCombinators . unpack <$> hGetContents h'

-----------------------------------------------------------------------------
-- Parsing the combinator file format

-- This must agree with the version in src/runtime/eval.c.
combVersion :: String
combVersion = "v9.3"

-- | Parse a serialized value, including the header, and the names
-- of the definitions, if present.
parseCombinators :: String -> Prog
parseCombinators s =
  case stripPrefix (combVersion ++ "\n") s of
    Just r | (n@(_:_), '\n':r') <- span isDigit r, all isDigit n ->
      case parseGraph r' of
        (Prog defs e, rest) -> renameLabels (parseNames rest) (Prog defs (dropAs e))
    _ -> error $ "Mhs.Reflect: unsupported combinator file format: " ++ show (takeWhile (/= '\n') s)

-- Parse the graph, and return what follows it.
parseGraph :: String -> (Prog, String)
parseGraph = p [] []
  where
    p :: [(Ident, Exp)] -> [Exp] -> String -> (Prog, String)
    p defs stk (' ':s) = p defs stk s
    p defs stk ('\n':s) = p defs stk s
    p defs (x:f:stk) ('@':s) = p defs (App f x : stk) s
    p defs [e] ('}':s) = (Prog (reverse defs) e, s)
    p defs stk ('#':'#':s) = lit (LInt64 . read) defs stk s
    p defs stk ('#':s) = lit (LInt . read) defs stk s
    p defs stk ('&':'&':s) = lit (LFloat . read) defs stk s
    p defs stk ('&':s) = lit (LDouble . read) defs stk s
    p defs stk ('%':s) =
      case span (/= '"') s of (i, _:r) -> p defs (Lit (LInteger (read i)) : stk) r
                              _ -> perr s
    p defs stk ('"':s) =
      case getByteString s of (bs, r) -> p defs (Lit (LBytes bs) : stk) r
    p defs stk ('$':s) =
      case span isDigit s of
        (n@(_:_), ' ':r) | (bs, r') <- splitAt (read n) r -> p defs (Lit (LBytes (pack bs)) : stk) r'
        _ -> perr s
    p defs stk ('!':'"':s) =
      case getByteString s of (bs, r) -> p defs (Lit (LTick (unpack bs)) : stk) r
    p defs stk ('^':s) = lit LFFI defs stk s
    p defs stk (';':s) = lit LFunPtr defs stk s
    p defs stk ('_':s) =
      case tok s of (l, r) -> p defs (Var ('_':l) : stk) r
    p defs (e:stk) (':':s) =
      case tok s of (l, r) -> p ((i, e) : defs) (Var i : stk) r
                                where i = '_':l
    p defs stk ('[':s) =
      case span isDigit s of
        (n@(_:_), ']':r) | k <- read n, length stk >= k ->
          p defs (Array (reverse (take k stk)) : drop k stk) r
        _ -> perr s
    p defs stk s@(_:_) = case tok s of (c, r) -> p defs (atom c : stk) r
    p _ _ s = perr s

    lit :: (String -> Literal) -> [(Ident, Exp)] -> [Exp] -> String -> (Prog, String)
    lit con defs stk s =
      case tok s of (sn, r) -> p defs (Lit (con sn) : stk) r

    tok = break isSpace
    perr s = error $ "Mhs.Reflect: parse error at " ++ show (take 20 s)

-- In a file written by mhs the graph ends with A d1 (A d2 (... (A dn e))),
-- where the di are the definitions.  Since A x y = y, that is just e.
dropAs :: Exp -> Exp
dropAs (App (App (Lit (LPrim "A")) _) e) = dropAs e
dropAs e = e

-- The names of the definitions, which mhs puts last in a .combffi file,
-- after the foreign imports:
--   #####Names
--   label name
--   ...
parseNames :: String -> [(Int, Ident)]
parseNames s =
  case dropWhile (/= "#####Names") (lines s) of
    _ : lns -> names lns
    [] -> []
  where
    names (ln : lns) | (l@(_:_), ' ':n@(_:_)) <- span isDigit ln = (read l, n) : names lns
    names _ = []

-- A label that has a name is renamed to it.
-- The label numbers of a serialized value depend on heap addresses, so they
-- differ between runs.  Rename the other labels to _1, _2, ... in the order
-- they are defined, which only depends on the shape of the graph.
renameLabels :: [(Int, Ident)] -> Prog -> Prog
renameLabels names (Prog defs e) = Prog [ (ren i, sub d) | (i, d) <- defs ] (sub e)
  where
    nums = [ read l | ('_':l, _) <- defs ] :: [Int]
    top = maximum (0 : nums)
    tbl = accumArray (\ _ x -> x) 0 (0, top) (zip nums [1 :: Int ..])
    named = accumArray (\ _ x -> Just x) Nothing (0, top) [ ln | ln@(l, _) <- names, l <= top ]
    ren ('_':l) | Just n <- named ! k = n
                | otherwise = '_' : show (tbl ! k)
                where k = read l
    ren i = i
    sub (Var i) = Var (ren i)
    sub (App f a) = App (sub f) (sub a)
    sub (Array es) = Array (map sub es)
    sub x = x

-- Combinators, primitives, and the data type combinators.
atom :: String -> Exp
atom s =
  case s of
    'C':'_':r | Just (k, n) <- twoNums r -> Con k n
    _ -> Lit (LPrim s)
  where
    twoNums r =
      case span isDigit r of
        (k@(_:_), '_':r') | (n@(_:_), "") <- span isDigit r' -> Just (read k, read n)
        _ -> Nothing

-- Get a quoted string, see doc/COMB.md for the escapes.
getByteString :: String -> (ByteString, String)
getByteString = get []
  where
    get r ('"':s) = (pack (reverse r), s)
    get r ('\\':'?':s) = get ('\x7f':r) s
    get r ('\\':'_':s) = get ('\xff':r) s
    get r ('\\':c:s) = get (c:r) s
    get r ('^' :c:s) | c < '\x40' = get (chr (ord c - 0x20) : r) s
                     | otherwise  = get (chr (ord c + 0x40) : r) s
    get r ('|' :c:s) = get (chr (ord c + 0x80) : r) s
    get r (c:s)     = get (c : r) s
    get _ [] = error "Mhs.Reflect: unterminated string"

-----------------------------------------------------------------------------
-- Decompilation

-- A state monad for generating fresh variable names.
newtype F a = F (Int -> (a, Int))

instance Functor F where
  fmap f (F g) = F $ \ n -> case g n of (a, n') -> (f a, n')
instance Applicative F where
  pure a = F $ \ n -> (a, n)
  (<*>) = ap
instance Monad F where
  F g >>= k = F $ \ n -> case g n of (a, n') -> case k a of F h -> h n'

runF :: F a -> a
runF (F g) = fst (g 1)

fresh :: Int -> F [Ident]
fresh k = F $ \ n -> (map (\ i -> 'x' : show i) [n .. n + k - 1], n + k)

-- The combinators, with their arity and reduction rule.
combinator :: String -> Maybe (Int, [Exp] -> Exp)
combinator c =
  case c of
    "S"   -> r 3 $ \ [x, y, z] -> x @@ z @@ (y @@ z)
    "K"   -> r 2 $ \ [x, _] -> x
    "I"   -> r 1 $ \ [x] -> x
    "B"   -> r 3 $ \ [x, y, z] -> x @@ (y @@ z)
    "C"   -> r 3 $ \ [x, y, z] -> x @@ z @@ y
    "S'"  -> r 4 $ \ [x, y, z, w] -> x @@ (y @@ w) @@ (z @@ w)
    "B'"  -> r 4 $ \ [x, y, z, w] -> x @@ y @@ (z @@ w)
    "C'"  -> r 4 $ \ [x, y, z, w] -> x @@ (y @@ w) @@ z
    "C'B" -> r 4 $ \ [x, y, z, w] -> x @@ z @@ (y @@ w)
    "A"   -> r 2 $ \ [_, y] -> y
    "U"   -> r 2 $ \ [x, y] -> y @@ x
    "Z"   -> r 3 $ \ [x, y, _] -> x @@ y
    "K2"  -> r 3 $ \ [x, _, _] -> x
    "K3"  -> r 4 $ \ [x, _, _, _] -> x
    "K4"  -> r 5 $ \ [x, _, _, _, _] -> x
    "P"   -> r 3 $ \ [x, y, z] -> z @@ x @@ y
    "R"   -> r 3 $ \ [x, y, z] -> y @@ z @@ x
    "O"   -> r 4 $ \ [x, y, _, w] -> w @@ x @@ y
    _     -> Nothing
  where r n f = Just (n, f)

infixl 9 @@
(@@) :: Exp -> Exp -> Exp
(@@) = App

apps :: Exp -> [Exp] -> Exp
apps = foldl App

spine :: Exp -> (Exp, [Exp])
spine = go []
  where go as (App f a) = go (a:as) f
        go as e = (e, as)

-- The case combinators: D_a0_..._a{m-1}, with the arities of the arms, and
-- E_k1_n1_..._km_nm, with the constructor numbers and arities of the arms.
data CaseComb = CaseD [Int] | CaseS [(Int, Int)]

caseComb :: String -> Maybe CaseComb
caseComb ('D':'_':r) | Just as <- nums r = Just (CaseD as)
caseComb ('E':'_':r) | Just kns <- nums r >>= pairs = Just (CaseS kns)
  where pairs (k : n : ns) = ((k, n) :) <$> pairs ns
        pairs [] = Just []
        pairs _ = Nothing
caseComb _ = Nothing

-- Numbers separated by _
nums :: String -> Maybe [Int]
nums r =
  case span isDigit r of
    (ds@(_:_), "")      -> Just [read ds]
    (ds@(_:_), '_':r') -> (read ds :) <$> nums r'
    _                   -> Nothing

-- The number of arguments of a case combinator, including the scrutinee.
caseArgs :: CaseComb -> Int
caseArgs (CaseD as) = length as + 1
caseArgs (CaseS kns) = length kns + 2

-- | Decompile combinator code to lambda expressions.
decompile :: Prog -> Prog
decompile (Prog defs e) = runF $ do
  defs' <- mapM (\ (i, d) -> (,) i <$> dec d) defs
  e' <- dec e
  return (Prog defs' e')

dec :: Exp -> F Exp
dec = go
  where
    go e =
      case spine e of
        (Lit (LPrim c), as)
          | Just (n, rule) <- combinator c ->
            if length as >= n then
              go (apps (rule (take n as)) (drop n as))
            else do
              -- Too few arguments, so eta expand.
              vs <- fresh (n - length as)
              b <- go (rule (as ++ map Var vs))
              return (foldr Lam b vs)
        (Lit (LPrim "Y"), f : as) -> do
          -- Y f = let r = f r in r
          v <- head <$> fresh 1
          b <- go (f @@ Var v)
          apps (Let [(v, b)] (Var v)) <$> mapM go as
        (Lit (LTick _), a : as) -> go (apps a as)
        (Lit (LPrim "fromUTF8"), Lit (LBytes bs) : as) ->
          apps (Lit (LString (T.unpack (decodeUtf8 bs)))) <$> mapM go as
        (Lit (LPrim c), as)
          | Just cc <- caseComb c -> do
            let n = caseArgs cc
            if length as < n then do
              -- The scrutinee (and maybe arms) are missing, so eta expand.
              vs <- fresh (n - length as)
              b <- go (apps e (map Var vs))
              return (foldr Lam b vs)
            else do
              let (cas, rest) = splitAt n as
              c' <- case cc of
                      CaseD ars -> dense ars cas
                      CaseS kns -> sparse kns cas
              apps c' <$> mapM go rest
        (f, as) -> apps f <$> mapM go as

    -- D_a0_..._a{m-1} f0 ... f{m-1} e
    dense ars cas = do
      let (arms, [scrut]) = splitAt (length cas - 1) cas
      arms' <- sequence (zipWith3 arm [0..] ars arms)
      s <- go scrut
      return (Case s arms' Nothing)

    -- E_k1_n1_..._km_nm d f1 ... fm e
    sparse kns (d : cas) = do
      let (arms, scrut) = splitAt (length kns) cas
      arms' <- zipWithM (uncurry arm) kns arms
      d' <- go d
      s <- go (head scrut)
      return (Case s arms' (Just d'))
    sparse _ [] = error "Mhs.Reflect: bad sparse case"

    -- An arm for constructor k with n fields.
    arm k n f = do
      vs <- fresh n
      b <- go (apps f (map Var vs))
      return (PCon k n vs, b)

-----------------------------------------------------------------------------
-- Printing, as Haskell code.  What fits on a line is printed on one line,
-- e.g., case x of { C_0_0 -> 0; C_1_2 y z -> y }, and otherwise a case or
-- let has a line for each arm or definition, still with the braces and
-- semicolons, and an application a line for each argument.

instance Pretty Exp where
  pPrintPrec _ d = ppr (if d <= 0 then 0 else if d <= appPrec then 1 else 2)

instance Pretty Prog where
  pPrint = pprProg

-- | A program as Haskell code, a let with a definition per line if it has
-- definitions.  There is no newline at the end.
ppProg :: Prog -> String
ppProg = render . pprProg

-- | An expression as Haskell code.
ppExp :: Exp -> String
ppExp = render . ppr 0

pprProg :: Prog -> Doc
pprProg (Prog [] b) = ppr 0 b
pprProg (Prog ies b) = vcat [text "let", nest 2 (vcat (map pprDef ies)), text "in " <+> ppr 0 b]

-- A definition.  The variables of a lambda are on the line of the name.
pprDef :: (Ident, Exp) -> Doc
pprDef (i, e@(Lam _ _)) = hang (ppIdent i <+> text "=" <+> h) 2 b where (h, b) = pprLam e
pprDef (i, e) = hang (ppIdent i <+> text "=") 2 (ppr 0 e)

-- The variables of a lambda, and its body.
pprLam :: Exp -> (Doc, Doc)
pprLam e = (text ("\\" ++ unwords vs ++ " ->"), ppr 0 b)
  where (vs, b) = lams e

-- Precedence: 0 anywhere, 1 function in an application, 2 argument.
ppr :: Int -> Exp -> Doc
ppr _ (Var i) = ppIdent i
ppr p (Lit l) = pprLit p l
ppr _ (Con k n) = conName k n
ppr _ (Array es) = text "[|" <> sep (punctuate (text ",") (map (ppr 0) es)) <> text "|]"
ppr p e@(Lam _ _) = maybeParens (p > 0) (hang h 2 b) where (h, b) = pprLam e
ppr p e@(App _ _) = maybeParens (p > 1) (sep (ppr 1 f : map (nest 2 . ppr 2) as)) where (f, as) = spine e
ppr p (Case s arms md) = maybeParens (p > 0) $
  block (text "case" <+> ppr 0 s <+> text "of") (map pprArm arms ++ [ arm (text "_") d | Just d <- [md] ])
  where pprArm (PCon k n vs, b) = arm (hsep (conName k n : [ text (if v `occursIn` b then v else "_") | v <- vs ])) b
        arm l b = hang (l <+> text "->") 2 (ppr 0 b)
ppr p (Let bs b) = maybeParens (p > 0) $
  sep [block (text "let") (map pprDef bs), text "in" <+> ppr 0 b]

-- A block, h { d1; ...; dn }, on one line, or with a line for each di.
block :: Doc -> [Doc] -> Doc
block h ds = sep ([h <+> text "{"] ++ map (nest 2) (punctuate (text ";") ds) ++ [text "}"])

pprLit :: Int -> Literal -> Doc
pprLit p l =
  case l of
    LInt i      -> num i
    LInt64 i    -> num i
    LDouble d   -> num d
    LFloat f    -> num f
    LInteger i  -> num i
    LBytes bs   -> text ("B" ++ show (unpack bs))
    LString s   -> text (show s)
    LPrim s     -> maybeParens (isOper s) (text s)
    LFFI s      -> text ("^" ++ s)
    LFunPtr s   -> text (";" ++ s)
    LTick s     -> text ("!" ++ show s)
  where num :: (Show a) => a -> Doc
        num x = let s = show x in maybeParens (p > 1 && take 1 s == "-") (text s)

conName :: Int -> Int -> Doc
conName k n = text ("C_" ++ show k ++ "_" ++ show n)

-- A variable, which can be the name of a definition, e.g., Data.List.++
ppIdent :: Ident -> Doc
ppIdent i = maybeParens (isOper i) (text i)

-- Operators, e.g., + and IO.>>=, are printed in parentheses.
-- A name that ends with a dot is the operator ., e.g., Data.Function..
isOper :: String -> Bool
isOper s = not (all (\ c -> isAlphaNum c || c == '_' || c == '\'' || c == '.') s) || "." `isSuffixOf` s

lams :: Exp -> ([Ident], Exp)
lams (Lam v e) = case lams e of (vs, b) -> (v:vs, b)
lams e = ([], e)

occursIn :: Ident -> Exp -> Bool
occursIn v = occ
  where
    occ (Var i) = i == v
    occ (Lam i e) = i /= v && occ e
    occ (App f a) = occ f || occ a
    occ (Case s arms md) = occ s || any (\ (PCon _ _ vs, b) -> v `notElem` vs && occ b) arms || maybe False occ md
    occ (Let bs b) = v `notElem` map fst bs && (any (occ . snd) bs || occ b)
    occ (Array es) = any occ es
    occ _ = False
