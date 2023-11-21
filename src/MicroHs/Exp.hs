{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE PatternSynonyms #-}
-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Exp(
  compileOpt,
--  compileOptX,
  substExp,
  Exp(..), toStringP,
  PrimOp,
  encodeString,
  app2, cCons, cNil, cFlip,
  allVarsExp, freeVars,
  encodeList,
  ) where
import Prelude --Xhiding((<>))
import Data.Char
import Data.List
import MicroHs.Ident
import MicroHs.Expr(Lit(..), showLit)
import Text.PrettyPrint.HughesPJ
--Ximport Control.DeepSeq
--Ximport Compat
import Debug.Trace

type PrimOp = String

--
-- Used combinators
--   * indicates that the implementation uses an indirection
--   A indicates allocation in the implementation
-- S  x y z   = x z (y z)             A
-- K  x y     = x                     *
-- I  x       = x                     *
-- B  x y z   = x (y z)               A
-- C  x y z   = x z y                 A
-- S' x y z w = x (y w) (z w)         A
-- B' x y z w = x y (z w)             A
-- C' x y z w = x (y w) z             A
-- A  x y     = y                     *
-- T  x y     = y x
-- n@(Y x)    = x n
-- Z x y z   = x y
-- P  x y z   = z x y                 A
-- R  x y z   = y z x                 A
-- O  x y z w = w x y                 A
--

data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Lit Lit

--Xinstance NFData Exp where rnf (Var i) = rnf i; rnf (App f a) = rnf f `seq` rnf a; rnf (Lam i e) = rnf i `seq` rnf e; rnf (Lit l) = rnf l

instance Eq Exp where
  (==) (Var i1)    (Var i2)    = i1 == i2
  (==) (App f1 a1) (App f2 a2) = f1 == f2 && a1 == a2
  (==) (Lam i1 e1) (Lam i2 e2) = i1 == i2 && e1 == e2
  (==) (Lit l1)    (Lit l2)    = l1 == l2
  (==) _           _           = False

data MaybeApp = NotApp | IsApp Exp Exp

getApp :: Exp -> MaybeApp
getApp ae =
  case ae of
    App f a -> IsApp f a
    _       -> NotApp

isPrim :: String -> Exp -> Bool
isPrim s ae =
  case ae of
    Lit (LPrim ss) -> s == ss
    _       -> False

isK :: Exp -> Bool
isK = isPrim "K"

isI :: Exp -> Bool
isI = isPrim "I"

isB :: Exp -> Bool
isB = isPrim "B"

isC :: Exp -> Bool
isC = isPrim "C"

isY :: Exp -> Bool
isY = isPrim "Y"

isP :: Exp -> Bool
isP = isPrim "P"

app2 :: Exp -> Exp -> Exp -> Exp
app2 f a1 a2 = App (App f a1) a2

app3 :: Exp -> Exp -> Exp -> Exp -> Exp
app3 f a1 a2 a3 = App (app2 f a1 a2) a3

cCons :: Exp
cCons = Lit (LPrim "O")

cNil :: Exp
cNil = Lit (LPrim "K")

cFlip :: Exp
cFlip = Lit (LPrim "C")

cId :: Exp
cId = Lit (LPrim "I")

cConst :: Exp
cConst = Lit (LPrim "K")

cSpread :: Exp
cSpread = Lit (LPrim "S")

cP :: Exp
cP = Lit (LPrim "P")

--cR :: Exp
--cR = Lit (LPrim "R")

-- Avoid quadratic concatenation by using difference lists,
-- turning concatenation into function composition.
toStringP :: Exp -> (String -> String)
toStringP ae =
  case ae of
    Var x   -> (showIdent x ++)
    Lit (LStr s) ->
      -- Encode very short string directly as combinators.
      if length s > 1 then
        (quoteString s ++)
      else
        toStringP (encodeString s)
    Lit (LInteger _) -> undefined
    Lit (LRat _) -> undefined
    Lit l   -> (showLit l ++)
    Lam x e -> (("(\\" ++ showIdent x ++ " ") ++) . toStringP e . (")" ++)
    App f a -> ("(" ++) . toStringP f . (" " ++) . toStringP a . (")" ++)

quoteString :: String -> String
quoteString s =
  let
    achar c =
      if c == '"' || c == '\\' || c < ' ' || c > '~' then
        '\\' : show (ord c) ++ ['&']
      else
        [c]
  in '"' : concatMap achar s ++ ['"']

encodeString :: String -> Exp
encodeString = encodeList . map (Lit . LInt . ord)

encodeList :: [Exp] -> Exp
encodeList = foldr (app2 cCons) cNil

compileOpt :: Exp -> Exp
compileOpt = improveT . compileExp

compileExp :: Exp -> Exp
compileExp ae =
  case ae of
    App f a -> App (compileExp f) (compileExp a)
    Lam x a -> abstract x a
    _       -> ae

abstract :: Ident -> Exp -> Exp
abstract x ae =
  case ae of
    Var y  -> if x == y then cId else cK (Var y)
    App f a -> cS (abstract x f) (abstract x a)
    Lam y e -> abstract x $ abstract y e
    Lit _ -> cK ae

cK :: Exp -> Exp
cK e  = App cConst e

cS :: Exp -> Exp -> Exp
cS a1 a2 =
 if isK a1 then cId else
  let
    r = cS2 a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp k1 e1 ->
        if isK k1 then
          case getApp a2 of
            IsApp k2 e2 ->
              if isK k2 then
                cK (App e1 e2)
              else
                cB e1 a2
            NotApp ->
              if isI a2 then
                e1
              else
                cB e1 a2
        else
          r
cS2 :: Exp -> Exp -> Exp
cS2 a1 a2 =
  case getApp a2 of
    NotApp -> cS3 a1 a2
    IsApp k2 e2 ->
      if isK k2 then
        cC a1 e2
      else
        cS3 a1 a2

cS3 :: Exp -> Exp -> Exp
cS3 a1 a2 =
  let
    r = app2 cSpread a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp be1 e2 ->
        case getApp be1 of
          NotApp -> r
          IsApp b1 e1 ->
            if isB b1 then
              cSS e1 e2 a2
            else
              r

{-
--cS e1 e2 | trace ("S (" ++ toString e1 ++ ") (" ++ toString e2 ++ ")") False = undefined
cS CK              _           = CI                -- S K e           = I
cS (App CK e1)     (App CK e2) = cK (App e1 e2)    -- S (K e1) (K e2) = K (e1 e2)
cS (App CK e1)     CI          = e1                -- S (K e1) I      = e1
cS (App CK e1)     e2          = cB e1 e2          -- S (K e1) e2     = B e1 e2
cS e1              (App CK e2) = cC e1 e2          -- S e1     (K e2) = C e1 e2
cS (App (App CB e1) e2) e3     = cSS e1 e2 e3      -- S (B e1 e2) e3  = S' e1 e2 e3
cS e1 e2                       = App2 CS e1 e2
-}

cC :: Exp -> Exp -> Exp
cC a1 e3 =
  let
    r = cC2 a1 e3
  in
    case getApp a1 of
      NotApp -> r
      IsApp x1 e2 ->
        case getApp x1 of
          NotApp -> r
          IsApp bc e1 ->
            if isB bc then
              cCC e1 e2 e3
            else if isC bc && isI e1 then
              app2 cP e2 e3
--            else if isC bc && isC e1 then
--              app2 cR e2 e3
            else
              r

cC2 :: Exp -> Exp -> Exp
cC2 a1 a2 = app2 cFlip a1 a2

{-
cC (App (App CB e1) e2) e3          = cCC e1 e2 e3      -- C (B e1 e2) e3  = C' e1 e2 e3
cC (Var op)             e2 | Just op' <- lookup op flipOps = App (Var op') e2 -- C op e = flip-op e
cC (App (App CC CI) e2) e3          = app2 CP e2 e3
cC (App (App CC CC) e2) e3          = app2 CR e2 e3
cC e1                   e2          = app2 CC e1 e2
-}

cB :: Exp -> Exp -> Exp
cB a1 a2 =
  let
    r = cB2 a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp cb ck ->
        if isB cb && isK ck && isP a2 then
          Lit (LPrim "O")
        else
          r

cB2 :: Exp -> Exp -> Exp
cB2 a1 a2 =
  let
    r = cB3 a1 a2
  in
    case getApp a2 of
      IsApp x1 x2 ->
        case getApp x1 of
          IsApp cb ck ->
            if isY a1 && isB cb && isK ck then
              x2
            else
              r
          NotApp ->
            if isC a1 && isC x1 && isI x2 then
              cP
            else
              r
      NotApp -> r

cB3 :: Exp -> Exp -> Exp
cB3 a1 a2 =
  if isI a1 then
    a2
  else
    app2 (Lit (LPrim "B")) a1 a2

{-
cB (App CB CK) CP             = CO -- Cons
cB CY          (App (App CB CK) e) = e  -- B Y (B K e) = e
cB CC          (App CC CI)    = CP -- Pair
cB CI          e              = e  -- B I e = e
cB e1          e2             = app2 CB e1 e2
-}

cSS :: Exp -> Exp -> Exp -> Exp
cSS e1 e2 e3 = app3 (Lit (LPrim "S'")) e1 e2 e3

cCC :: Exp -> Exp -> Exp -> Exp
cCC e1 e2 e3 = app3 (Lit (LPrim "C'")) e1 e2 e3

improveT :: Exp -> Exp
improveT ae =
  case getApp ae of
    NotApp -> ae
    IsApp f a ->
      let
        ff = improveT f
        aa = improveT a
      in
        if isK ff && isI aa then
          Lit (LPrim "A")
{- Using I x --> x does not improve things.
        else if isI ff then
          aa
-}
        else if isB ff && isK aa then
          Lit (LPrim "Z")
        else if isC ff && isI aa then
          Lit (LPrim "U")
        else if isB ff && isB aa then
          Lit (LPrim "B'")
        else if isC ff && isC aa then
          Lit (LPrim "R")
        else
          let
            def =
              case getApp aa of
                IsApp ck e ->
                  if isY ff && isK ck then
                    e
                  else
                    App ff aa
                NotApp -> App ff aa
          in
            def
{-
            case getApp ff of
              IsApp xf xa ->
                if isK xf then
                  xa
                else
                  def
              NotApp -> def
-}
            
{-
-- K I      -->  A
-- C I      -->  T
-- B B      -->  B'
-- Y (K e)  -->  e
-- K x y    -->  x
improveT (App f a) =
  case (improveT f, improveT a) of
    (CK,                     CI) -> CA
--    (CI,                      e) -> e
    (CY,               App CK e) -> e
--    (App CK e1,              e2) -> e1
    (e1,                     e2) -> App e1 e2
improveT e = e
-}

instance Show Exp where
  show = render . ppExp

ppExp :: Exp -> Doc
ppExp ae =
  case ae of
--    Let i e b -> sep [ text "let" <+> ppIdent i <+> text "=" <+> ppExp e, text "in" <+> ppExp b ]
    Var i -> ppIdent i
    App f a -> parens $ ppExp f <+> ppExp a
    Lam i e -> parens $ text "\\" <> ppIdent i <> text "." <+> ppExp e
    Lit l -> text (showLit l)

substExp :: Ident -> Exp -> Exp -> Exp
substExp si se ae =
  case ae of
    Var i -> if i == si then se else ae
    App f a -> App (substExp si se f) (substExp si se a)
    Lam i e -> if si == i then
                 ae
               else if elem i (freeVars se) then
                 let
                   fe = allVarsExp e
                   ase = allVarsExp se
                   j = head [ v | n <- enumFrom (0::Int), let { v = mkIdent ("a" ++ show n) }, not (elem v ase), not (elem v fe) ]
                 in
                   --trace ("substExp " ++ unwords [si, i, j]) $
                   Lam j (substExp si se (substExp i (Var j) e))
               else
                   Lam i (substExp si se e)
    Lit _ -> ae

freeVars :: Exp -> [Ident]
freeVars ae =
  case ae of
    Var i -> [i]
    App f a -> freeVars f ++ freeVars a
    Lam i e -> deleteAllBy (==) i (freeVars e)
    Lit _ -> []

allVarsExp :: Exp -> [Ident]
allVarsExp ae =
  case ae of
    Var i -> [i]
    App f a -> allVarsExp f ++ allVarsExp a
    Lam i e -> i : allVarsExp e
    Lit _ -> []

--------
-- Possible additions
--
-- Added:
--  R = C C
--  R x y z = (C C x y) z = C y x z = y z x
--
--  Q = C I
--  Q x y z = (C I x y) z = I y x z = y x z
--
-- Added:
--  Z = B K
--  Z x y z = B K x y z = K (x y) z = x y
--
--  ZK = Z K
--  ZK x y z = Z K x y z = (K x) z = x
--
--  C'B = C' B
--  C'B x y z w = C' B x y z w = B (x z) y w = x z (y w)

--  B (B e) x y z = B e (x y) z = e (x y z)
--
--  B' :: (a -> b -> c) -> a -> (d -> b) -> d -> c
--  B' k f g x = k f (g x)
--
-- Common:
--  817: C' B
--  616: B Z
--  531: C' C
--  352: Z K
--  305: C' S
--
--  BZ = B Z
--  BZ x y z w = B Z x y z w = Z (x y) z w = x y z
--
--  C'C = C' C
--  C'C x y z w = C' C x y z w = C (x z) y w = x z w y


---------------------------------------------------------------

{-
-- Oleg's abstraction algorithm

data Peano = S Peano | Z
  --Xderiving (Show)
data DB = N Peano | L DB | A DB DB | Free Ident | K Lit
  --Xderiving (Show)

index :: Ident -> [Ident] -> Maybe Peano
index x xs = lookupBy eqIdent x $ zip xs $ iterate S Z

deBruijn :: Exp -> DB
deBruijn = go [] where
  go binds e =
    case e of
      Var x -> maybe (Free x) N $ index x binds
      App t u -> A (go binds t) (go binds u)
      Lam x t -> L $ go (x:binds) t
      Lit l -> K l

type CL = Exp
type BCL = ([Bool], CL)

com :: String -> Exp
com s = Lit (LPrim s)
(@@) :: Exp -> Exp -> Exp
(@@) f a = App f a

convertBool :: (BCL -> BCL -> CL) -> DB -> BCL
convertBool (#) ee =
  case ee of
    N Z -> (True:[], com "I")
    N (S e) -> (False:g, d) where (g, d) = rec $ N e
    L e -> case rec e of
             ([], d) -> ([], com "K" @@ d)
             (False:g, d) -> (g, ([], com "K") # (g, d))
             (True:g, d) -> (g, d)
    A e1 e2 -> (zipWithDefault False (||) g1 g2, t1 # t2) where
      t1@(g1, _) = rec e1
      t2@(g2, _) = rec e2
    Free s -> ([], Var s)
    K l -> ([], Lit l)
  where rec = convertBool (#)

optEta :: DB -> BCL
optEta = convertBool (#) where
  (#) ([], d1)           {- # -} ([],       d2)      = d1 @@ d2
  (#) ([], d1)           {- # -} (True:[],  Lit (LPrim "I")) = d1
  (#) ([], d1)           {- # -} (True:g2,  d2)      = ([], com "B" @@ d1) # (g2, d2)
  (#) ([], d1)           {- # -} (False:g2, d2)      = ([], d1) # (g2, d2)
  (#) (True:[], Lit (LPrim "I")) {- # -} ([],       d2)      = com "U" @@ d2
  (#) (True:[], Lit (LPrim "I")) {- # -} (False:g2, d2)      = ([], com "U") # (g2, d2)
  (#) (True:g1, d1)      {- # -} ([],       d2)      = ([], com "R" @@ d2) # (g1, d1)
  (#) (True:g1, d1)      {- # -} (True:g2,  d2)      = (g1, ([], com "S") # (g1, d1)) # (g2, d2)
  (#) (True:g1, d1)      {- # -} (False:g2, d2)      = (g1, ([], com "C") # (g1, d1)) # (g2, d2)
  (#) (False:g1, d1)     {- # -} ([],       d2)      = (g1, d1) # ([], d2)
  (#) (False:_g1, d1)    {- # -} (True:[],  Lit (LPrim "I")) = d1
  (#) (False:g1, d1)     {- # -} (True:g2,  d2)      = (g1, ([], com "B") # (g1, d1)) # (g2, d2)
  (#) (False:g1, d1)     {- # -} (False:g2, d2)      = (g1, d1) # (g2, d2)

zipWithDefault :: forall a b . a -> (a -> a -> b) -> [a] -> [a] -> [b]
zipWithDefault d f     []     ys = map (f d) ys
zipWithDefault d f     xs     [] = map (flip f d) xs
zipWithDefault d f (x:xt) (y:yt) = f x y : zipWithDefault d f xt yt

compileOptX :: Exp -> Exp
compileOptX = snd . optEta . deBruijn
-}

