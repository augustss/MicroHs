import BasicTypes
import TcTerm
import TcMonad

env :: [(Name,Sigma)]
env =
  [("not",  boolType --> boolType)
  ,("C",    (ForAll [tvx] (tx --> tx)) --> TyCon "T")
  ,("pair", ForAll [tvx,tvy] (tx --> ty --> TyApp (TyApp (TyCon "Pair") tx) ty))
  ]

tvx, tvy :: TyVar
tvx = BoundTv "x"
tvy = BoundTv "y"
tx, ty :: Type
tx = TyVar tvx
ty = TyVar tvy

tc :: Term -> IO Sigma
tc e = do
  et <- runTc env (typecheck e) 
  case et of
    Left msg -> error $ docToString msg
    Right t  -> return t

pp :: Outputable a => a -> IO ()
pp = putStrLn . docToString . ppr

tcpp :: Term -> IO ()
tcpp e = do
  pp e
  t <- tc e
  pp t

main :: IO ()
main = do
  tcpp e1
  tcpp e2
  tcpp e3
  tcpp e4

_tv :: String -> Type
_tv = TyVar . BoundTv

e1 :: Term
e1 = Lam "x" $ Var "x"

e2 :: Term
e2 = Ann (Lam "x" $ Var "x") (ForAll [tvx] (tx --> tx))

e3 :: Term
e3 = Lam "b" $ If (App (Var "not") (Var "b")) e1 e2

e4 :: Term
e4 = PLam (PCon "C" [PVar "f"]) $ App (App (Var "pair") (App (Var "f") (LitI 1))) (App (Var "f") (LitB True))
