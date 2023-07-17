-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Exp(module MicroHs.Exp) where
import Prelude
import MicroHs.Parse --X(Ident, eqIdent)
--Ximport Compat
--import Debug.Trace

type PrimOp = String

data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Int Int
  | Prim PrimOp
  --Xderiving (Show, Eq)

data MaybeApp = NotApp | IsApp Exp Exp

getApp :: Exp -> MaybeApp
getApp ae =
  case ae of
    App f a -> IsApp f a
    _       -> NotApp

getVar :: Exp -> Maybe Ident
getVar ae =
  case ae of
    Var v -> Just v
    _     -> Nothing

isPrim :: String -> Exp -> Bool
isPrim s ae =
  case ae of
    Prim ss -> eqString s ss
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
cCons = Prim "O"

cNil :: Exp
cNil = Prim "K"

cFlip :: Exp
cFlip = Prim "C"

eqExp :: Exp -> Exp -> Bool
eqExp ae1 ae2 =
  case ae1 of
    Var i1 ->
      case ae2 of
        Var i2 -> eqIdent i1 i2
        _ -> False
    App e11 e12 ->
      case ae2 of
        App e21 e22 -> eqExp e11 e21 && eqExp e12 e22
        _ -> False
    Lam i1 e1 ->
      case ae2 of
        Lam i2 e2 -> eqIdent i1 i2 && eqExp e1 e2
        _ -> False
    Int i1 ->
      case ae2 of
        Int i2 -> i1 == i2
        _ -> False
    Prim p1 ->
      case ae2 of
        Prim p2 -> eqString p1 p2
        _ -> False

toStringP :: Exp -> String
toStringP ae =
  case ae of
    Var x   -> x
    Prim x  -> '$':x
    Int i   -> showInt i
    Lam x e -> "(\\" ++ x ++ " " ++ toStringP e ++ ")"
    App f a -> "(" ++ toStringP f ++ " " ++ toStringP a ++ ")"

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
    Var y  -> if eqString x y then Prim "I" else cK (Var y)
    App f a -> cS (abstract x f) (abstract x a)
    Lam y e -> abstract x $ abstract y e
    Prim _ -> cK ae
    Int _ -> cK ae

cK :: Exp -> Exp
cK e  = App (Prim "K") e

cS :: Exp -> Exp -> Exp
cS a1 a2 =
 if isK a1 then Prim "I" else
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
    r = app2 (Prim "S") a1 a2
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
              app2 (Prim "P") e2 e3
            else
              r

cC2 :: Exp -> Exp -> Exp
cC2 a1 a2 =
  let
    r = app2 (Prim "C") a1 a2
  in
    case getVar a1 of
      Nothing -> r
      Just op ->
        case lookupBy eqString op flipOps of
          Just oq -> App (Var oq) a2
          Nothing -> r
{-
cC (App (App CB e1) e2) e3          = cCC e1 e2 e3      -- C (B e1 e2) e3  = C' e1 e2 e3
cC (Var op)             e2 | Just op' <- lookup op flipOps = App (Var op') e2 -- C op e = flip-op e
cC (App (App CC CI) e2) e3          = app2 CP e2 e3
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
          Prim "O"
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
              Prim "P"
            else
              r
      NotApp -> r

cB3 :: Exp -> Exp -> Exp
cB3 a1 a2 =
  if isI a1 then
    a2
  else
    app2 (Prim "B") a1 a2

{-
cB (App CB CK) CP             = CO -- Cons
cB CY          (App (App CB CK) e) = e  -- B Y (B K e) = e
cB CC          (App CC CI)    = CP -- Pair
cB CI          e              = e  -- B I e = e
cB e1          e2             = app2 CB e1 e2
-}

cSS :: Exp -> Exp -> Exp -> Exp
cSS e1 e2 e3 = app3 (Prim "S'") e1 e2 e3

cCC :: Exp -> Exp -> Exp -> Exp
cCC e1 e2 e3 = app3 (Prim "C'") e1 e2 e3

-- This is a hack, it assumes things about the Prelude
flipOps :: [(PrimOp, PrimOp)]
flipOps =
  [("Prelude.+",  "Prelude.+")
  ,("Prelude.-",  "Prelude.subtract")
  ,("Prelude.*",  "Prelude.*")
  ,("Prelude.==", "Prelude.==")
  ,("Prelude./=", "Prelude./=")
  ,("Prelude.<",  "Prelude.>")
  ,("Prelude.<=", "Prelude.>=")
  ,("Prelude.>",  "Prelude.<")
  ,("Prelude.>=", "Prelude.<=")
  ]

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
          Prim "T"
        else
          case getApp aa of
            NotApp -> App ff aa
            IsApp ck e ->
              if isY ff && isK ck then
                e
              else
                App ff aa
{-
improveT (App f a) =
  case (improveT f, improveT a) of
    (CK,                     CI) -> CT
    (CY,               App CK e) -> e
    (e1,                     e2) -> App e1 e2
improveT e = e
-}
