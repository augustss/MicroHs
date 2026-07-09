{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MicroHs.EncodeData(
  SPat(..),
  encConstr,
  encCase,
  encIf,
  encList,
  encTuple,
  encTupleSel,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.Ord(comparing)
import Data.List(sortBy)
import MicroHs.Exp
import MicroHs.Expr(Con(..), Lit(..), impossible)
import MicroHs.Ident
import Debug.Trace

--
-- Encoding of constructors and case
--

data SPat = SPat Con [Ident]    -- simple pattern
--  deriving(Show, Eq)

encCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCase var pes@((SPat (ConData cs _ _) _, _) : _) dflt =
  let m = length cs                    -- number of constructors
      a = length pes                   -- number of present arms
      encCaseDense = apps (cCaseD m) (var : map arm cs)
        where arm (c, k) =
                head $ [ lams xs e | (SPat (ConData _ i _) xs, e) <- pes, c == i ] ++
                       [ lams (replicate k dummyIdent) dflt ]
        
  in  if m > 5 && 3*m `quot` 4 > a then  -- more than 5 constructors and less than 75% filled
        encCaseSparse var pes dflt
      else
        encCaseDense
encCase _ _ _ = impossible

encCaseSparse :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCaseSparse var pes dflt =
  --trace ("sparse " ++ show (m, a)) encCaseDense
  let pes' = sortBy (comparing fst) [(conNo c, lams xs e) | (SPat c xs, e) <- pes]
      arm (i, e) = App (Lit (LInt i)) e
  in  apps (cCaseS (length pes)) (var : dflt : map arm pes')


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
      strict (True :ys) (x:is) e = app2 (Lit (LPrim "seq")) (Var x) (strict ys is e)
      strict _          _      e = e
  in  lams xs $ strict ss xs $ apps (cCon k n) (map Var xs)

-- Constructor k, with n arguments
encConstrLazy :: Int -> Int -> Exp
encConstrLazy k n = cCon k n

-- Special case of encCase
encIf :: Exp -> Exp -> Exp -> Exp
encIf c t e = apps (cCaseD 2) [c, e, t]

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
  in  apps (cCaseD 1) [tup, foldr Lam (Var (xs !! m)) xs]

cCaseD :: Int -> Exp
cCaseD m = Lit $ LPrim $ "D_" ++ show m

cCaseS :: Int -> Exp
cCaseS m = Lit $ LPrim $ "E_" ++ show m

cCon :: Int -> Int -> Exp
cCon k n = Lit $ LPrim $ "C_" ++ show k ++ "_" ++ show n

conNo :: Con -> Int
conNo (ConData cks i _) = length $ takeWhile ((/= i) . fst) cks
conNo _ = undefined
