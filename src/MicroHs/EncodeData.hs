{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MicroHs.EncodeData(
  SPat(..),
  encConstr,
  encCase,
  encIf,
  encList,
  encTuple,
  encTupleSel,
  getEncIf,
  maxConstructors,
  maxFields,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.Ord(comparing)
import Data.List(sortBy)
import MicroHs.Error(mhsError)
import MicroHs.Exp
import MicroHs.Expr(Con(..), Lit(..), PrimOp(..), impossible)
import MicroHs.Ident
--import Debug.Trace

--
-- Encoding of constructors and case
--
-- The scrutinee is the last argument, so that \x -> case x of ... eta reduces.
--   Dense:   D_a0_..._a{m-1} f0 ... f{m-1} e      (ak is the number of fields of constructor k)
--   Sparse:  E_m d A_k1_n1 f1 ... A_km_nm fm e    (arms sorted by constructor number)
-- The runtime does not need the arities ak, but they make it possible to
-- turn the combinator code back into lambda expressions.

-- The runtime keeps a constructor number, and the number of arms of a case,
-- in 11 bits, and the number of fields in 8 bits (see src/runtime/eval.c).
-- The type checker checks data types against these limits; the checks
-- below catch anything else, e.g., very big tuples.
maxConstructors :: Int
maxConstructors = 2047

maxFields :: Int
maxFields = 255

checkCons :: Int -> a -> a
checkCons m a | m > maxConstructors = mhsError $ "too many constructors, " ++ show m ++ ", the maximum is " ++ show maxConstructors
              | otherwise = a

checkFields :: Int -> a -> a
checkFields n a | n > maxFields = mhsError $ "too many fields in a constructor or tuple, " ++ show n ++ ", the maximum is " ++ show maxFields
                | otherwise = a

data SPat = SPat Con [Ident]    -- simple pattern
--  deriving(Show, Eq)

-- The exact point where we switch from dense to sparse case isn't critical.
-- It's roughly the same around the current point.
encCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCase var pes@((SPat (ConData cs _ _) _, _) : _) dflt =
  let m = length cs                    -- number of constructors
      a = length pes                   -- number of present arms
      encCaseDense = apps (cCaseD (map snd cs)) (map arm cs ++ [var])
        where arm (c, k) =
                head $ [ lams xs e | (SPat (ConData _ i _) xs, e) <- pes, c == i ] ++
                       [ lams (replicate k dummyIdent) dflt ]

  in  if m > 4 && 3*m > 4*a then  -- more than 4 constructors and less than 75% (=3/4) filled
        encCaseSparse var pes dflt
      else
        encCaseDense
encCase _ _ _ = impossible

encCaseSparse :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCaseSparse var pes dflt =
  --trace ("sparse " ++ show (m, a)) encCaseDense
  let pes' = sortBy (comparing fst) [(conNo c, (xs, e)) | (SPat c xs, e) <- pes]
      arm (k, (xs, e)) = [cArm k (length xs), lams xs e]
  in  apps (cCaseS (length pes)) (dflt : concatMap arm pes' ++ [var])


-- constructor k out of m
encConstr :: Int -> Int -> [Bool] -> Exp
encConstr k _m ss | or ss     = encConstrStrict k ss
                  | otherwise = encConstrLazy   k (length ss)

-- constructor k out of m
encConstrStrict :: Int -> [Bool] -> Exp
encConstrStrict k ss =
  let n = length ss         -- constructor arity
      xs = [mkIdent ("$x" ++ show j) | j <- [0 .. n-1] ]
      strict (False:ys) (_:is) e = strict ys is e
      strict (True :ys) (x:is) e = app2 (litPrim "seq") (Var x) (strict ys is e)
      strict _          _      e = e
  in  lams xs $ strict ss xs $ apps (cCon k n) (map Var xs)

-- Constructor k, with n arguments
encConstrLazy :: Int -> Int -> Exp
encConstrLazy k n = cCon k n

-- Special case of encCase
encIf :: Exp -> Exp -> Exp -> Exp
encIf c t e = apps (cCaseD [0, 0]) [e, t, c]

-- Recognize an encoded if, see encIf.
getEncIf :: Exp -> Maybe (Exp, Exp, Exp)
getEncIf (App (App (App (Lit (LPrim (PCaseD [0, 0]))) e) t) c) = Just (c, t, e)
getEncIf _ = Nothing

encList :: [Exp] -> Exp
encList = foldr (app2 cCons) cNil

cNil :: Exp
cNil = encConstrLazy 0 0

cCons :: Exp
cCons = encConstrLazy 1 2

encTuple :: [Exp] -> Exp
encTuple es = apps (encConstrLazy 0 (length es)) es

encTupleSel :: Int -> Int -> Exp -> Exp
encTupleSel m n tup =
  let xs = [mkIdent ("x" ++ show i) | i <- [1 .. n] ]
  in  apps (cCaseD [n]) [foldr Lam (Var (xs !! m)) xs, tup]

-- A dense case, given the number of fields of each constructor.
cCaseD :: [Int] -> Exp
cCaseD as = checkCons (length as) $ foldr checkFields (Lit $ LPrim $ PCaseD as) as

cCaseS :: Int -> Exp
cCaseS m = checkCons m $ Lit $ LPrim $ PCaseS m

cCon :: Int -> Int -> Exp
cCon k n = checkCons (k + 1) $ checkFields n $ Lit $ LPrim $ PCon k n

cArm :: Int -> Int -> Exp
cArm k n = checkCons (k + 1) $ checkFields n $ Lit $ LPrim $ PArm k n

conNo :: Con -> Int
conNo (ConData cks i _) = length $ takeWhile ((/= i) . fst) cks
conNo _ = undefined
