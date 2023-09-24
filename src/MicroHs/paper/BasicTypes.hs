module BasicTypes where
import Prelude hiding ((<>))
-- This module defines the basic types used by the type checker
-- Everything defined in here is exported
import Text.PrettyPrint.HughesPJ
import Data.IORef
import Data.List( nub )
import Data.Maybe( fromMaybe )

infixr 4 --> -- The arrow type constructor
infixl 4 `App` -- Application

-----------------------------------
-- Ubiquitous types --
-----------------------------------
type Name = String -- Names are very simple

-----------------------------------
-- Expressions --
-----------------------------------
-- Examples below
data Term = Var Name -- x
  | Lit Int -- 3
  | App Term Term -- f x
  | Lam Name Term -- \ x -> x
  | ALam Name Sigma Term -- \ x -> x
  | Let Name Term Term -- let x = f y in x+1
  | Ann Term Sigma -- (f x) :: Int

atomicTerm :: Term -> Bool
atomicTerm (Var _) = True
atomicTerm (Lit _) = True
atomicTerm _ = False

-----------------------------------
-- Types --
-----------------------------------
type Sigma = Type
type Rho = Type -- No top-level ForAll
type Tau = Type -- No ForAlls anywhere

data Type = ForAll [TyVar] Rho -- Forall type
  | Fun Type Type -- Function type
  | TyCon TyCon -- Type constants
  | TyVar TyVar -- Always bound by a ForAll
  | MetaTv MetaTv -- A meta type variable

data TyVar
  = BoundTv String -- A type variable bound by a ForAll
  | SkolemTv String Uniq -- A skolem constant; the String is
                         -- just to improve error messages

data MetaTv = Meta Uniq TyRef -- Can unify with any tau-type
type TyRef = IORef (Maybe Tau)
             -- 'Nothing' means the type variable is not substituted
             -- 'Just ty' means it has been substituted by 'ty'

instance Eq MetaTv where
  (Meta u1 _) == (Meta u2 _) = u1 == u2

instance Eq TyVar where
  (BoundTv s1) == (BoundTv s2) = s1 == s2
  (SkolemTv _ u1) == (SkolemTv _ u2) = u1 == u2
  _ == _ = False

type Uniq = Int

data TyCon = IntT | BoolT
  deriving( Eq )

---------------------------------
-- Constructors
(-->) :: Sigma -> Sigma -> Sigma
arg --> res = Fun arg res
intType, boolType :: Tau
intType = TyCon IntT
boolType = TyCon BoolT

---------------------------------
-- Free and bound variables
metaTvs :: [Type] -> [MetaTv]
-- Get the MetaTvs from a type; no duplicates in result
metaTvs tys = foldr go [] tys
  where
    go (MetaTv tv) acc
      | tv `elem` acc = acc
      | otherwise = tv : acc
    go (TyVar _) acc = acc
    go (TyCon _) acc = acc
    go (Fun arg res) acc = go arg (go res acc)
    go (ForAll _ ty) acc = go ty acc -- ForAll binds TyVars only

freeTyVars :: [Type] -> [TyVar]
-- Get the free TyVars from a type; no duplicates in result
freeTyVars tys = foldr (go []) [] tys
  where
    go :: [TyVar] -- Ignore occurrences of bound type variables
       -> Type -- Type to look at
       -> [TyVar] -- Accumulates result
       -> [TyVar]
    go bound (TyVar tv) acc
      | tv `elem` bound = acc
      | tv `elem` acc = acc
      | otherwise = tv : acc
    go bound (MetaTv _) acc = acc
    go bound (TyCon _) acc = acc
    go bound (Fun arg res) acc = go bound arg (go bound res acc)
    go bound (ForAll tvs ty) acc = go (tvs ++ bound) ty acc

tyVarBndrs :: Rho -> [TyVar]
-- Get all the binders used in ForAlls in the type, so that
-- when quantifying an outer for-all we can avoid these inner ones
tyVarBndrs ty = nub (bndrs ty)
  where
    bndrs (ForAll tvs body) = tvs ++ bndrs body
    bndrs (Fun arg res) = bndrs arg ++ bndrs res
    bndrs _ = []

tyVarName :: TyVar -> String
tyVarName (BoundTv n) = n
tyVarName (SkolemTv n _) = n

---------------------------------
-- Substitution
type Env = [(TyVar, Tau)]

substTy :: [TyVar] -> [Type] -> Type -> Type

-- Replace the specified quantified type variables by
-- given meta type variables
-- No worries about capture, because the two kinds of type
-- variable are distinct
substTy tvs tys ty = subst_ty (tvs `zip` tys) ty
subst_ty :: Env -> Type -> Type
subst_ty env (Fun arg res) = Fun (subst_ty env arg) (subst_ty env res)
subst_ty env (TyVar n) = fromMaybe (TyVar n) (lookup n env)
subst_ty env (MetaTv tv) = MetaTv tv
subst_ty env (TyCon tc) = TyCon tc
subst_ty env (ForAll ns rho) = ForAll ns (subst_ty env' rho)
  where
    env' = [(n,ty') | (n,ty') <- env, not (n `elem` ns)]

-----------------------------------
-- Pretty printing class --
-----------------------------------
class Outputable a where
  ppr :: a -> Doc

docToString :: Doc -> String
docToString = render

dcolon, dot :: Doc
dcolon = text "::"
dot = char '.'

-------------- Pretty-printing terms ---------------------
instance Outputable Term where
  ppr (Var n) = pprName n
  ppr (Lit i) = int i
  ppr (App e1 e2) = pprApp (App e1 e2)
  ppr (Lam v e) = sep [char '\\' <> pprName v <> text ".", ppr e]
  ppr (ALam v t e) = sep [char '\\' <> parens (pprName v <> dcolon <> ppr t)
                          <> text ".", ppr e]
  ppr (Let v rhs b) = sep [text "let {",
                           nest 2 (pprName v <+> equals <+> ppr rhs <+> char '}') ,
                           text "in",
                           ppr b]
  ppr (Ann e ty) = pprParendTerm e <+> dcolon <+> pprParendType ty

instance Show Term where
  show t = docToString (ppr t)

pprParendTerm :: Term -> Doc
pprParendTerm e | atomicTerm e = ppr e
                | otherwise = parens (ppr e)

pprApp :: Term -> Doc
pprApp e = go e []
  where
    go (App e1 e2) es = go e1 (e2:es)
    go e' es = pprParendTerm e' <+> sep (map pprParendTerm es)

pprName :: Name -> Doc
pprName n = text n

-------------- Pretty-printing types ---------------------
instance Outputable Type where
  ppr ty = pprType topPrec ty

instance Outputable MetaTv where
  ppr (Meta u _) = text "$" <> int u

instance Outputable TyVar where
  ppr (BoundTv n) = text n
  ppr (SkolemTv n u) = text n <+> int u

instance Show Type where
  show t = docToString (ppr t)

type Precedence = Int

topPrec, arrPrec, tcPrec, atomicPrec :: Precedence
topPrec = 0 -- Top-level precedence
arrPrec = 1 -- Precedence of (a->b)
tcPrec = 2 -- Precedence of (T a b)
atomicPrec = 3 -- Precedence of t

precType :: Type -> Precedence
precType (ForAll _ _) = topPrec
precType (Fun _ _) = arrPrec
precType _ = atomicPrec -- All the types are be atomic

pprParendType :: Type -> Doc
pprParendType ty = pprType tcPrec ty

pprType :: Precedence -> Type -> Doc
-- Print with parens if precedence arg > precedence of type itself
pprType p ty | p >= precType ty = parens (ppr_type ty)
             | otherwise = ppr_type ty

ppr_type :: Type -> Doc -- No parens
ppr_type (ForAll ns ty) = sep [text "forall" <+>
                               hsep (map ppr ns) <> dot,
                               ppr ty]
ppr_type (Fun arg res) = sep [pprType arrPrec arg <+> text "->",
                              pprType (arrPrec-1) res]
ppr_type (TyCon tc) = ppr_tc tc
ppr_type (TyVar n) = ppr n
ppr_type (MetaTv tv) = ppr tv

ppr_tc :: TyCon -> Doc
ppr_tc IntT = text "Int"
ppr_tc BoolT = text "Bool"
