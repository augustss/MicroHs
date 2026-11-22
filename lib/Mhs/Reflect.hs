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
module Mhs.Reflect(
  Ident, Exp(..), Literal(..), Pat(..), Prog(..),
  reflect, reflectCombinators,
  parseCombinators, decompile,
  ppProg, ppExp,
  ) where
import Control.Monad
import Data.ByteString.Char8(unpack, pack, ByteString)
import Data.Char(isAlphaNum, isDigit, isSpace, chr, ord)
import Data.Int(Int64)
import Data.List(stripPrefix)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Mhs.Array(accumArray, (!))
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

-----------------------------------------------------------------------------
-- Parsing the combinator file format

-- This must agree with the version in src/runtime/eval.c.
combVersion :: String
combVersion = "v9.3"

-- | Parse a serialized value, including the header.
parseCombinators :: String -> Prog
parseCombinators s =
  case stripPrefix (combVersion ++ "\n") s of
    Just r | (n@(_:_), '\n':r') <- span isDigit r, all isDigit n -> renameLabels (parseGraph r')
    _ -> error $ "Mhs.Reflect: unsupported combinator file format: " ++ show (takeWhile (/= '\n') s)

parseGraph :: String -> Prog
parseGraph = p [] []
  where
    p :: [(Ident, Exp)] -> [Exp] -> String -> Prog
    p defs stk (' ':s) = p defs stk s
    p defs stk ('\n':s) = p defs stk s
    p defs (x:f:stk) ('@':s) = p defs (App f x : stk) s
    p defs [e] ('}':_) = Prog (reverse defs) e
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

    lit :: (String -> Literal) -> [(Ident, Exp)] -> [Exp] -> String -> Prog
    lit con defs stk s =
      case tok s of (sn, r) -> p defs (Lit (con sn) : stk) r

    tok = break isSpace
    perr s = error $ "Mhs.Reflect: parse error at " ++ show (take 20 s)

-- The label numbers depend on heap addresses, so they differ between runs.
-- Rename the labels to _1, _2, ... in the order they are defined, which only
-- depends on the shape of the graph.
renameLabels :: Prog -> Prog
renameLabels (Prog defs e) = Prog [ (ren i, sub d) | (i, d) <- defs ] (sub e)
  where
    nums = [ read l | ('_':l, _) <- defs ] :: [Int]
    tbl = accumArray (\ _ x -> x) 0 (0, maximum (0 : nums)) (zip nums [1 :: Int ..])
    ren ('_':l) = '_' : show (tbl ! read l)
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
      arms' <- sequence (zipWith (\ (k, n) f -> arm k n f) kns arms)
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
-- Printing

ppProg :: Prog -> String
ppProg (Prog [] b) = ppExp b
ppProg (Prog ies b) = unlines $
  ["let"] ++ map (\ (i, e) -> "  " ++ i ++ " = " ++ ppExp e) ies ++ ["in  " ++ ppExp b]

ppExp :: Exp -> String
ppExp e = pp 0 e ""

-- Precedence: 0 anywhere, 1 function in an application, 2 argument.
pp :: Int -> Exp -> ShowS
pp _ (Var i) = showString i
pp p (Lit l) = ppLit p l
pp _ (Con k n) = showString ("C_" ++ show k ++ "_" ++ show n)
pp _ (Array es) = showString "[|" . commas (map (pp 0) es) . showString "|]"
pp p e@(Lam _ _) = paren (p > 0) $
  let (vs, b) = lams e
  in  showString "\\" . showString (unwords vs) . showString " -> " . pp 0 b
pp p (App f a) = paren (p > 1) $ pp 1 f . showString " " . pp 2 a
pp p (Case s arms md) = paren (p > 0) $
  showString "case " . pp 0 s . showString " of { " .
  semis (map ppArm arms ++ maybe [] (\ d -> [showString "_ -> " . pp 0 d]) md) .
  showString " }"
  where ppArm (PCon k n vs, b) =
          showString ("C_" ++ show k ++ "_" ++ show n) .
          foldr (\ v r -> showString " " . showString (if v `occursIn` b then v else "_") . r) id vs .
          showString " -> " . pp 0 b
pp p (Let bs b) = paren (p > 0) $
  showString "let { " . semis (map (\ (i, e) -> showString i . showString " = " . pp 0 e) bs) .
  showString " } in " . pp 0 b

ppLit :: Int -> Literal -> ShowS
ppLit p l =
  case l of
    LInt i      -> num i
    LInt64 i    -> num i
    LDouble d   -> num d
    LFloat f    -> num f
    LInteger i  -> num i
    LBytes bs   -> showString "B" . shows (unpack bs)
    LString s   -> shows s
    LPrim s     -> paren (isOper s) (showString s)
    LFFI s      -> showString ("^" ++ s)
    LFunPtr s   -> showString (";" ++ s)
    LTick s     -> showString ("!" ++ show s)
  where num :: (Show a) => a -> ShowS
        num x = let s = show x in paren (p > 1 && take 1 s == "-") (showString s)

-- Operators, e.g., + and IO.>>=, are printed in parentheses.
isOper :: String -> Bool
isOper = not . all (\ c -> isAlphaNum c || c == '_' || c == '\'' || c == '.')

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

paren :: Bool -> ShowS -> ShowS
paren True s = showString "(" . s . showString ")"
paren False s = s

semis :: [ShowS] -> ShowS
semis [] = id
semis (s:ss) = s . foldr (\ t r -> showString "; " . t . r) id ss

commas :: [ShowS] -> ShowS
commas [] = id
commas (s:ss) = s . foldr (\ t r -> showString ", " . t . r) id ss
