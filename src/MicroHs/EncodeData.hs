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
encCase var pes dflt | useDirect n (length pes) || True = encCaseDirect var pes dflt
                     | otherwise                = encCaseNo var pes dflt
  where n = numConstr pes

-- Encode constructor k of n, the number of arguments
-- is given by the length of the strictness list.
encConstr :: Int -> Int -> [Bool] -> Exp
encConstr cno numCon ss =
  let
    xs = [mkIdent ("$x" ++ show j) | (j, _) <- zip [0::Int ..] ss]
    strict (False:ys) (_:is) e = strict ys is e
    strict (True:ys)  (x:is) e = app2 (Lit (LPrim "seq")) (Var x) (strict ys is e)
    strict _ _ e = e
    con = encConstrNo cno numCon (length ss)
  in
    if all not ss then
      con                   -- just an optimization to avoid a lot of eta reductions
    else
      lams xs $ strict ss xs $ apps con (map Var xs)

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

encConstrNo :: Int -> Int -> Int -> Exp
encConstrNo cno numCon numArg = Lit $ LPrim scon
  where
    scon =
      case (cno, numCon, numArg) of
        (0, 1, 0) -> "I"       -- single nullary
        (0, 1, 1) -> "U"       -- single unary
        (0, 1, 2) -> "P"       -- pair
        (0, 2, 0) -> "K"       -- []/False/Nothing
        (1, 2, 0) -> "A"       -- True
--      (1, 2, 1) -> "Z U@"    -- Just/Right
        (1, 2, 2) -> "O"       -- (:)
        _ -> "M" ++ show cno ++ "_" ++ show numCon ++ "_" ++ show numArg

encCaseDirect :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCaseDirect var pes dflt =
  let cs = dataInfo pes
      arm (c, k) =
        head $ [ lams xs e | (SPat (ConData _ i _) xs, e) <- pes, c == i ] ++
               [ lams (replicate k dummyIdent) dflt ]
  in  apps var (map arm cs)

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
   -- extract arguments to constructor
   -- Uk (\ i1 ... ik -> rhs) e
   match :: Exp -> [Ident] -> Exp -> Exp
   match e is rhs = App (App (unpack (length is)) (lams is rhs)) e

   unpack 2 = Lit $ LPrim "U"                -- use fast special case for pairs
   unpack _ = Lit $ LPrim $ "unp"
{-
   cmpInt :: Exp -> Int -> Exp
   cmpInt x i = app2 (Lit (LPrim "icmp")) x (Lit (LInt i))
   encTri o l e h = app3 o l e h
-}

conNo :: Con -> Int
conNo (ConData cks i _) = length $ takeWhile ((/= i) . fst) cks
conNo _ = undefined

dataInfo :: [(SPat, Exp)] -> [(Ident, Int)]
dataInfo ((SPat (ConData cs _ _) _, _):_) = cs
dataInfo _ = undefined

numConstr :: [(SPat, Exp)] -> Int
numConstr = length . dataInfo

-- Make a tuple
encTuple :: [Exp] -> Exp
encTuple es = apps (encConstrNo 0 1 (length es)) es

-- Select component m from an n-tuple
encTupleSel :: Int -> Int -> Exp -> Exp
encTupleSel m n tup =
  let
    xs = [mkIdent ("x" ++ show i) | i <- [1 .. n] ]
  in App tup (foldr Lam (Var (xs !! m)) xs)

