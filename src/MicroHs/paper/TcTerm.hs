module TcTerm where
import BasicTypes
import Data.IORef
import TcMonad
import Data.List( (\\) )
import Text.PrettyPrint.HughesPJ

------------------------------------------
-- The top-level wrapper --
------------------------------------------
typecheck :: Term -> Tc Sigma
typecheck e = do { ty <- inferSigma e
                 ; zonkType ty }

-----------------------------------
-- The expected type --
-----------------------------------
data Expected a = Infer (IORef a) | Check a

------------------------------------------
-- tcRho, and its variants --
------------------------------------------
checkRho :: Term -> Rho -> Tc ()
-- Invariant: the Rho is always in weak-prenex form
checkRho expr ty = tcRho expr (Check ty)

inferRho :: Term -> Tc Rho
inferRho expr
  = do { ref <- newTcRef (error "inferRho: empty result")
       ; tcRho expr (Infer ref)
       ; readTcRef ref }

tcRho :: Term -> Expected Rho -> Tc ()
-- Invariant: if the second argument is (Check rho),
-- then rho is in weak-prenex form
tcRho (LitI _) exp_ty
  = instSigma intType exp_ty
tcRho (LitB _) exp_ty
  = instSigma boolType exp_ty
tcRho (Var v) exp_ty
  = do { v_sigma <- lookupVar v
       ; instSigma v_sigma exp_ty }
tcRho (App fun arg) exp_ty
  = do { fun_ty <- inferRho fun
       ; (arg_ty, res_ty) <- unifyFun fun_ty
       ; checkSigma arg arg_ty
       ; instSigma res_ty exp_ty }
tcRho (Lam var body) (Check exp_ty)
  = do { (var_ty, body_ty) <- unifyFun exp_ty
       ; extendVarEnv var var_ty (checkRho body body_ty) }
tcRho (Lam var body) (Infer ref)
  = do { var_ty <- newTyVarTy
       ; body_ty <- extendVarEnv var var_ty (inferRho body)
       ; writeTcRef ref (var_ty --> body_ty) }
tcRho (ALam var var_ty body) (Check exp_ty)
  = do { (arg_ty, body_ty) <- unifyFun exp_ty
       ; subsCheck arg_ty var_ty
       ; extendVarEnv var var_ty (checkRho body body_ty) }
tcRho (ALam var var_ty body) (Infer ref)
  = do { body_ty <- extendVarEnv var var_ty (inferRho body)
       ; writeTcRef ref (var_ty --> body_ty) }
tcRho (Let var rhs body) exp_ty
  = do { var_ty <- inferSigma rhs
       ; extendVarEnv var var_ty (tcRho body exp_ty) }
tcRho (Ann body ann_ty) exp_ty
  = do { checkSigma body ann_ty
       ; instSigma ann_ty exp_ty }
tcRho (If e1 e2 e3) (Check exp_ty)  -- This?
  = do { checkRho e1 boolType
       ; checkSigma e2 exp_ty
       ; checkSigma e3 exp_ty }
tcRho (If e1 e2 e3) (Infer ref)
  = do { checkRho e1 boolType
       ; rho1 <- inferRho e2
       ; rho2 <- inferRho e3
       ; subsCheck rho1 rho2
       ; subsCheck rho2 rho1
       ; writeTcRef ref rho1 }
tcRho (PLam pat body) (Infer ref)
  = do { (binds, pat_ty) <- inferPat pat
       ; body_ty <- extendVarEnvList binds (inferRho body)
       ; writeTcRef ref (pat_ty --> body_ty) }
tcRho (PLam pat body) (Check ty)
  = do { (arg_ty, res_ty) <- unifyFun ty
       ; binds <- checkPat pat arg_ty
       ; extendVarEnvList binds (checkRho body res_ty) }

tcPat :: Pat -> Expected Sigma -> Tc [(Name,Sigma)]
tcPat PWild _exp_ty = return []
tcPat (PVar v) (Infer ref) = do { ty <- newTyVarTy
                                ; writeTcRef ref ty
                                ; return [(v,ty)] }
tcPat (PVar v) (Check ty) = return [(v, ty)]
tcPat (PAnn p pat_ty) exp_ty = do { binds <- checkPat p pat_ty
                                  ; instPatSigma pat_ty exp_ty
                                  ; return binds }
tcPat (PCon con ps) exp_ty
  = do { (arg_tys, res_ty) <- instDataCon con
       ; envs <- mapM check_arg (ps `zip` arg_tys)
       ; instPatSigma res_ty exp_ty
       ; return (concat envs) }
  where
    check_arg (p,ty) = checkPat p ty

instPatSigma :: Sigma -> Expected Sigma -> Tc ()
instPatSigma pat_ty (Infer ref) = writeTcRef ref pat_ty
instPatSigma pat_ty (Check exp_ty) = subsCheck exp_ty pat_ty

checkPat :: Pat -> Sigma -> Tc [(Name, Sigma)]
checkPat p exp_ty = tcPat p (Check exp_ty)

inferPat :: Pat -> Tc ([(Name, Sigma)], Sigma)
inferPat pat
  = do { ref <- newTcRef (error "inferPat: empty result")
       ; binds <- tcPat pat (Infer ref)
       ; ty <- readTcRef ref
       ; return (binds, ty) }

instDataCon :: Name -> Tc ([Sigma], Tau)
instDataCon c = do
  v_sigma <- lookupVar c
  v_sigma' <- instantiate v_sigma
  return (argsAndRes v_sigma')

argsAndRes :: Rho -> ([Sigma], Tau)
argsAndRes (Fun arg_ty res_ty) = (arg_ty : arg_tys, res_ty') where (arg_tys, res_ty') = argsAndRes res_ty
argsAndRes t = ([], t)

------------------------------------------
-- inferSigma and checkSigma
------------------------------------------

inferSigma :: Term -> Tc Sigma
inferSigma e
  = do { exp_ty <- inferRho e
       ; env_tys <- getEnvTypes
       ; env_tvs <- getMetaTyVars env_tys
       ; res_tvs <- getMetaTyVars [exp_ty]
       ; let forall_tvs = res_tvs \\ env_tvs
       ; quantify forall_tvs exp_ty }

checkSigma :: Term -> Sigma -> Tc ()
checkSigma expr sigma
  = do { (skol_tvs, rho) <- skolemise sigma
       ; checkRho expr rho
       ; env_tys <- getEnvTypes
       ; esc_tvs <- getFreeTyVars (sigma : env_tys)
       ; let bad_tvs = filter (`elem` esc_tvs) skol_tvs
       ; check (null bad_tvs)
         (text "Type not polymorphic enough") }

------------------------------------------
-- Subsumption checking --
------------------------------------------
subsCheck :: Sigma -> Sigma -> Tc ()
-- (subsCheck args off exp) checks that
-- 'off' is at least as polymorphic as 'args -> exp'
subsCheck sigma1 sigma2 -- Rule DEEP-SKOL
  = do { (skol_tvs, rho2) <- skolemise sigma2
       ; subsCheckRho sigma1 rho2
       ; esc_tvs <- getFreeTyVars [sigma1,sigma2]
       ; let bad_tvs = filter (`elem` esc_tvs) skol_tvs
       ; check (null bad_tvs)
         (vcat [text "Subsumption check failed:",
                nest 2 (ppr sigma1),
                text "is not as polymorphic as",
                nest 2 (ppr sigma2)])
       }

subsCheckRho :: Sigma -> Rho -> Tc ()
-- Invariant: the second argument is in weak-prenex form
subsCheckRho sigma1@(ForAll _ _) rho2 -- Rule SPEC
  = do { rho1 <- instantiate sigma1
       ; subsCheckRho rho1 rho2 }
subsCheckRho rho1 (Fun a2 r2) -- Rule FUN
  = do { (a1,r1) <- unifyFun rho1; subsCheckFun a1 r1 a2 r2 }
subsCheckRho (Fun a1 r1) rho2 -- Rule FUN
  = do { (a2,r2) <- unifyFun rho2; subsCheckFun a1 r1 a2 r2 }
subsCheckRho tau1 tau2 -- Rule MONO
  = unify tau1 tau2 -- Revert to ordinary unification
subsCheckFun :: Sigma -> Rho -> Sigma -> Rho -> Tc ()
subsCheckFun a1 r1 a2 r2
  = do { subsCheck a2 a1 ; subsCheckRho r1 r2 }
instSigma :: Sigma -> Expected Rho -> Tc ()
-- Invariant: if the second argument is (Check rho),
-- then rho is in weak-prenex form
instSigma t1 (Check t2) = subsCheckRho t1 t2
instSigma t1 (Infer r) = do { t1' <- instantiate t1
                            ; writeTcRef r t1' }
