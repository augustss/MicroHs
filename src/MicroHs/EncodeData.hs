module MicroHs.EncodeData(
  SPat(..),
  encConstr,
  encCase,
  encIf,
  encList,
  ) where
import Prelude
import MicroHs.Exp
import MicroHs.Expr(Con(..), Lit(..))
import MicroHs.Ident

--
-- Encoding of constructors and case
--

data SPat = SPat Con [Ident]    -- simple pattern
  deriving(Show, Eq)

encCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCase = encCaseScott
encConstr :: Int -> Int -> [Bool] -> Exp
encConstr = encConstrScott
encIf :: Exp -> Exp -> Exp -> Exp
encIf = encIfScott

-- Encode a case expression:
--  case var of p1->e1; p2->e2; ...; _->dflt
-- Assumes Scott encoding of data types.
encCaseScott :: Exp -> [(SPat, Exp)] -> Exp -> Exp
encCaseScott var pes dflt =
  --trace ("mkCase " ++ show pes) $
  case pes of
    (SPat (ConData cs _) _, _) : _ ->
      let
        arm (c, k) =
          let
            (vs, rhs) = head $ [ (xs, e) | (SPat (ConData _ i) xs, e) <- pes, c == i ] ++
                               [ (replicate k dummyIdent, dflt) ]
          in lams vs rhs
      in  apps var (map arm cs)
    _ -> undefined

-- Encode a constructor with strictness flags ss.
-- The constructor arity is given by ss, and the constructor number is i out of n.
-- Assumes Scott encoding of data types.
encConstrScott :: Int -> Int -> [Bool] -> Exp
encConstrScott i n ss =
  let
    f = mkIdent "$f"
    fs = map (\ k -> if k == i then f else dummyIdent) [0::Int .. n-1]
    xs = [mkIdent ("$x" ++ show j) | (j, _) <- zip [0::Int ..] ss]
    strict (False:ys) (_:is) e = strict ys is e
    strict (True:ys)  (x:is) e = App (App (Lit (LPrim "seq")) (Var x)) (strict ys is e)
    strict _ _ e = e
  in lams xs $ strict ss xs $ lams fs $ apps (Var f) (map Var xs)  

encIfScott :: Exp -> Exp -> Exp -> Exp
encIfScott c t e = app2 c e t

encList :: [Exp] -> Exp
encList = foldr (app2 cCons) cNil

-- XXX could use encConstr
cCons :: Exp
cCons = Lit (LPrim "O")

-- XXX could use encConstr
cNil :: Exp
cNil = Lit (LPrim "K")

