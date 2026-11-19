module MicroHs.Abstract(
  compileOpt,
  -- reduce,
  ) where
import qualified Prelude(); import MHSPrelude
import MicroHs.Ident
import MicroHs.Exp
import MicroHs.Expr(Lit(..), PrimOp(..))

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
-- U  x y     = y x
-- n@(Y x)    = x n
-- Z  x y z   = x y
-- P  x y z   = z x y                 A
-- R  x y z   = y z x                 A
-- O  x y z w = w x y                 A
-- K2 x y z   = x                     *
-- K3 x y z w = x                     *
-- K4 x y z w v = x                   *
-- C'B x y z w = x z (y w)            A
--

data MaybeApp = NotApp | IsApp Exp Exp

getApp :: Exp -> MaybeApp
getApp ae =
  case ae of
    App f a -> IsApp f a
    _       -> NotApp

isK :: Exp -> Bool
isK ae =
  case ae of
    Lit (LPrim PK) -> True
    _ -> False

isI :: Exp -> Bool
isI ae =
  case ae of
    Lit (LPrim PI) -> True
    _ -> False

isB :: Exp -> Bool
isB ae =
  case ae of
    Lit (LPrim PB) -> True
    _ -> False

isC :: Exp -> Bool
isC ae =
  case ae of
    Lit (LPrim PC) -> True
    _ -> False

isC' :: Exp -> Bool
isC' ae =
  case ae of
    Lit (LPrim PC') -> True
    _ -> False

isY :: Exp -> Bool
isY ae =
  case ae of
    Lit (LPrim PY) -> True
    _ -> False

isP :: Exp -> Bool
isP ae =
  case ae of
    Lit (LPrim PP) -> True
    _ -> False

isZ :: Exp -> Bool
isZ ae =
  case ae of
    Lit (LPrim PZ) -> True
    _ -> False

isK2 :: Exp -> Bool
isK2 ae =
  case ae of
    Lit (LPrim PK2) -> True
    _ -> False

isK3 :: Exp -> Bool
isK3 ae =
  case ae of
    Lit (LPrim PK3) -> True
    _ -> False

cI :: Exp
cI = Lit (LPrim PI)

cK :: Exp
cK = Lit (LPrim PK)

cS :: Exp
cS = Lit (LPrim PS)

cP :: Exp
cP = Lit (LPrim PP)

cC :: Exp
cC = Lit (LPrim PC)

--------------------

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
    Var y  -> if x == y then cI else aK (Var y)
    App f a -> aS (abstract x f) (abstract x a)
    Lam y e -> abstract x $ abstract y e
    Lit _ -> aK ae

aK :: Exp -> Exp
aK e  = App cK e

aS :: Exp -> Exp -> Exp
aS a1 a2 =
 if isK a1 then cI else
  let
    r = aS2 a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp k1 e1 ->
        if isK k1 then
          case getApp a2 of
            IsApp k2 e2 ->
              if isK k2 then
                aK (App e1 e2)
              else
                aB e1 a2
            NotApp ->
              if isI a2 then
                e1
              else
                aB e1 a2
        else
          r
aS2 :: Exp -> Exp -> Exp
aS2 a1 a2 =
  case getApp a2 of
    NotApp -> aS3 a1 a2
    IsApp k2 e2 ->
      if isK k2 then
        aC a1 e2
      else
        aS3 a1 a2

aS3 :: Exp -> Exp -> Exp
aS3 a1 a2 =
  let
    r = app2 cS a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp be1 e2 ->
        case getApp be1 of
          NotApp -> r
          IsApp b1 e1 ->
            if isB b1 then
              aS' e1 e2 a2
            else
              r

{-
--aS e1 e2 | trace ("S (" ++ toString e1 ++ ") (" ++ toString e2 ++ ")") False = undefined
aS CK              _           = CI                -- S K e           = I
aS (App CK e1)     (App CK e2) = aK (App e1 e2)    -- S (K e1) (K e2) = K (e1 e2)
aS (App CK e1)     CI          = e1                -- S (K e1) I      = e1
aS (App CK e1)     e2          = cB e1 e2          -- S (K e1) e2     = B e1 e2
aS e1              (App CK e2) = aC e1 e2          -- S e1     (K e2) = C e1 e2
aS (App (App CB e1) e2) e3     = aS' e1 e2 e3      -- S (B e1 e2) e3  = S' e1 e2 e3
aS e1 e2                       = App2 CS e1 e2
-}

aC :: Exp -> Exp -> Exp
aC a1 e3 =
  let
    r = aC2 a1 e3
  in
    case getApp a1 of
      NotApp -> r
      IsApp x1 e2 ->
        case getApp x1 of
          NotApp -> r
          IsApp bc e1 ->
            if isB bc then
              aC' e1 e2 e3
            else if isC bc && isI e1 then
              app2 cP e2 e3
--            else if isC bc && isC e1 then
--              app2 cR e2 e3
            else
              r

aC2 :: Exp -> Exp -> Exp
aC2 a1 a2 = app2 cC a1 a2

{-
aC (App (App CB e1) e2) e3          = aC' e1 e2 e3      -- C (B e1 e2) e3  = C' e1 e2 e3
aC (Var op)             e2 | Just op' <- lookup op flipOps = App (Var op') e2 -- C op e = flip-op e
aC (App (App C' CI) e2) e3          = app2 CP e2 e3
aC (App (App C' C') e2) e3          = app2 CR e2 e3
aC e1                   e2          = app2 C' e1 e2
-}

aB :: Exp -> Exp -> Exp
aB a1 a2 =
  let
    r = aB2 a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp cb ck ->
        if isB cb && isK ck && isP a2 then
          Lit (LPrim PO)
        else
          r

aB2 :: Exp -> Exp -> Exp
aB2 a1 a2 =
  let
    r = aB3 a1 a2
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

aB3 :: Exp -> Exp -> Exp
aB3 a1 a2 =
  if isI a1 then
    a2
  else
    app2 (Lit (LPrim PB)) a1 a2

{-
aB (App CB CK) CP             = CO -- Cons
aB CY          (App (App CB CK) e) = e  -- B Y (B K e) = e
aB C'          (App C' CI)    = CP -- Pair
aB CI          e              = e  -- B I e = e
aB e1          e2             = app2 CB e1 e2
-}

aS' :: Exp -> Exp -> Exp -> Exp
aS' e1 e2 e3 = app3 (Lit (LPrim PS')) e1 e2 e3

aC' :: Exp -> Exp -> Exp -> Exp
aC' e1 e2 e3 = app3 (Lit (LPrim PC')) e1 e2 e3

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
          Lit (LPrim PA)
{- Using I x --> x does not improve things.
        else if isI ff then
          aa
-}
        else if isB ff && isK aa then
          Lit (LPrim PZ)
        else if isC ff && isI aa then
          Lit (LPrim PU)
        else if isB ff && isB aa then
          Lit (LPrim PB')
        else if isC ff && isC aa then
          Lit (LPrim PR)
        else if isC' ff && isB aa then
          Lit (LPrim PC'B)
        else if isZ ff && isK aa then
          Lit (LPrim PK2)
        else if isZ ff && isK2 aa then
          Lit (LPrim PK3)
        else if isZ ff && isK3 aa then
          Lit (LPrim PK4)
        else
          let
            def =
              case getApp aa of
                IsApp ck e ->
                  if isY ff && isK ck then
                    e
                  else
                    kApp ff aa
                NotApp -> kApp ff aa
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


kApp :: Exp -> Exp -> Exp
kApp f@(Lit (LPrim PK)) a@(App (Lit (LPrim k)) x) =
  case k of
    PK  -> App (Lit (LPrim PK2)) x
    PK2 -> App (Lit (LPrim PK3)) x
    PK3 -> App (Lit (LPrim PK4)) x
    _   -> App f a
kApp f a = App f a

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
--
--  C'B P x y z w = P y (x z) w = w y (x z)

{- This makes very little difference.
   There are only 2 reductions in the entire compiler.
reduce :: Exp -> Exp
reduce e = red e []
  where
    isPrim p (Lit (LPrim p')) = p == p'
    isPrim _ _ = False
    -- No duplication, nor cycles, so no S, S', Y
    red (App f a) as                      = red f (reduce a : as)
    red f (x:as)         | isI          f && xxx "I" = red x as
    red f (x:y:as)       | isPrim PA   f && xxx "A" = red y as
                         | isPrim PU   f && xxx "U" = red y (x : as)
                         | isK          f && xxx "K" = red x as
    red f (x:y:z:as)     | isB          f && xxx "B" = red x (App y z : as)
                         | isC          f && xxx "C" = red x (z : y : as)
                         | isPrim PZ   f && xxx "Z" = red x (y : as)
                         | isP          f && xxx "P" = red z (x : y : as)
                         | isPrim PR   f && xxx "R" = red y (z : x : as)
                         | isPrim PK2  f && xxx "K2" = red x as
    red f (x:y:z:w:as)   | isPrim PB'  f && xxx "B'" = red x (y : App z w : as)
                         | isPrim PC'  f && xxx "C'" = red x (App y w : z : as)
                         | isPrim PO   f && xxx "O" = red w (x : y : as)
                         | isPrim PK3  f && xxx "K3" = red x as
                         | isPrim PC'B f && xxx "C'B" = red x (z : App y w : as)
    red f (x:_:_:_:_:as) | isPrim PK4  f && xxx "K4" = red x as

    red f as                              = foldl App f as

    xxx s = trace s True
-}
