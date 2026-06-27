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
import MicroHs.Exp
import MicroHs.Expr(Con(..), Lit(..), impossible)
import MicroHs.Ident

--
-- Encoding of constructors and case
--

data SPat = SPat Con [Ident]    -- simple pattern
--  deriving(Show, Eq)

encCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCase var pes@((SPat (ConData cs _ _) _, _) : _) dflt =
  let m = length cs                    -- number of constructors
      arm (c, k) =
          head $ [ lams xs e | (SPat (ConData _ i _) xs, e) <- pes, c == i ] ++
                 [ lams (replicate k dummyIdent) dflt ]
  in  apps (cCase m) (var : map arm cs)
encCase _ _ _ = impossible

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

-- Constructor k out of m
encConstrLazy :: Int -> Int -> Exp
encConstrLazy k n = cCon k n

-- Special case of encCase
encIf :: Exp -> Exp -> Exp -> Exp
encIf c t e = apps (cCase 2) [c, t, e]

encList :: [Exp] -> Exp
encList = foldr (app2 cCons) cNil

cNil :: Exp
cNil = encConstrLazy 0 2

cCons :: Exp
cCons = encConstrLazy 1 2

-- XXX get rid of replicate
encTuple :: [Exp] -> Exp
encTuple es = apps (encConstrLazy 0 1) es

encTupleSel :: Int -> Int -> Exp -> Exp
encTupleSel m n tup =
  let xs = [mkIdent ("x" ++ show i) | i <- [1 .. n] ]
  in  apps (cCase 1) [tup, foldr Lam (Var (xs !! m)) xs]

cCase :: Int -> Exp
cCase m = Lit $ LPrim $ "D_" ++ show m

cCon :: Int -> Int -> Exp
cCon k n = Lit $ LPrim $ "C_" ++ show k ++ "_" ++ show n
