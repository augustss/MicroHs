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
import Data.List
import MicroHs.Exp
import MicroHs.Expr(Con(..), Lit(..), impossible)
import MicroHs.Ident

--
-- Encoding of constructors and case
--

data SPat = SPat Con [Ident]    -- simple pattern
--  deriving(Show, Eq)

encCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCase var pes dflt | useDirect n (length pes) = encCaseDirect var pes dflt
                     | otherwise                = encCaseNo var pes dflt
  where n = numConstr pes

-- Encode constructor k of n, the number of arguments
-- is given by the length of the list
encConstr :: Int -> Int -> [Bool] -> Exp
encConstr k n ss = encConstrNo k n ss

-- Decide if we are doing a direct dispatch or a comparison tree.
useDirect :: Int -> Int -> Bool
useDirect numCon numArm = numCon < 5            -- few constructors
                       || numArm * 3 > numCon   -- more than 1/3 of the arms are present

encIf :: Exp -> Exp -> Exp -> Exp
encIf c t e = app2 c e t

encList :: [Exp] -> Exp
encList = foldr (app2 cCons) cNil

cCons :: Exp
cCons = encConstr 1 2 [False, False]

cNil :: Exp
cNil = encConstr 0 2 []

-------------------------------------------

encConstrNo :: Int -> Int -> [Bool] -> Exp
encConstrNo i n ss =
  let
    xs = [mkIdent ("$x" ++ show j) | (j, _) <- zip [0::Int ..] ss]
    strict (False:ys) (_:is) e = strict ys is e
    strict (True:ys)  (x:is) e = app2 (Lit (LPrim "seq")) (Var x) (strict ys is e)
    strict _ _ e = e
    con = Lit $ LPrim $ "M" ++ show i ++ "_" ++ show n ++ "_" ++ show (length ss)
  in
    if all not ss then
      con                   -- just an optimization to avoid a lot of eta reductions
    else
      lams xs $ strict ss xs $ apps con (map Var xs)

encCaseDirect :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCaseDirect var pes dflt =
  --trace ("mkCase " ++ show pes) $
  case pes of
    (SPat (ConData cs _ _) _, _) : _ ->
      let
        arm (c, k) =
          head $ [ lams xs e | (SPat (ConData _ i _) xs, e) <- pes, c == i ] ++
                 [ lams (replicate k dummyIdent) dflt ]
      in  apps var (map arm cs)
    _ -> undefined

encCaseNo :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCaseNo var@(Var _) pes dflt =
  App (Lam n $ caseTree (Var n) var 0 (numConstr pes) pes' dflt) (App getConNo var)
  where n = mkIdent "$n"
        pes' = sortBy (\ (x, _, _) (y, _, _) -> compare x y)
                      [(conNo c, xs, e) | (SPat c xs, e) <- pes]
        getConNo = Lit $ LPrim "cno"
encCaseNo _ _ _ = undefined  -- this shouldn't happen

caseTree :: Exp -> Exp -> Int -> Int -> [(Int, [Ident], Exp)] -> Exp -> Exp
caseTree n var lo hi pes dflt =
  case pes of
    [] -> dflt
    [(i, xs, e)] | hi - lo == 1 -> match var xs e
                 | otherwise    -> encIf (eqInt n i) (match var xs e) dflt
{-
    -- Strangely, this slows things down.
    -- Why?  A 3-way branch should be better than a 2-way.
    [(i, xs, e), (_, xs', e')]
                 | hi - lo == 2 -> encIf (eqInt n i) (match tup xs e) (match tup xs' e')
      let (pesl, (i, xs, e) : pesh) = splitAt (length pes `quot` 2) pes
      in  encTri (cmpInt n i) (caseTree n tup lo i pesl dflt)
                              (match tup xs e)
                              (caseTree n tup (i+1) hi pesh dflt)
-}
    _ ->
      case splitAt (length pes `quot` 2) pes of
        (pesl, pesh@((i, _, _):_)) ->
          encIf (ltInt n i) (caseTree n var lo i pesl dflt) (caseTree n var i hi pesh dflt)
        _ -> impossible
 where
   eqInt :: Exp -> Int -> Exp
   eqInt x i = app2 (Lit (LPrim "==")) x (Lit (LInt i))
   ltInt :: Exp -> Int -> Exp
   ltInt x i = app2 (Lit (LPrim "<")) x (Lit (LInt i))
   match :: Exp -> [Ident] -> Exp -> Exp
   match e is rhs = App (App (unpack (length is)) e) (lams is rhs)
   unpack l = Lit $ LPrim $ "U" ++ show l
{-
   cmpInt :: Exp -> Int -> Exp
   cmpInt x i = app2 (Lit (LPrim "icmp")) x (Lit (LInt i))
   encTri o l e h = app3 o l e h
-}

conNo :: Con -> Int
conNo (ConData cks i _) = length $ takeWhile ((/= i) . fst) cks
conNo _ = undefined

numConstr :: [(SPat, Exp)] -> Int
numConstr ((SPat (ConData cs _ _) _, _):_) = length cs
numConstr _ = undefined

-- Make a tuple
encTuple :: [Exp] -> Exp
encTuple = Lam f . foldl App (Var f)
  where f = mkIdent "$f"

-- Select component m from an n-tuple
encTupleSel :: Int -> Int -> Exp -> Exp
encTupleSel m n tup =
  let
    xs = [mkIdent ("x" ++ show i) | i <- [1 .. n] ]
  in App tup (foldr Lam (Var (xs !! m)) xs)

