module MicroHs.Fixity(resolveFixity) where
import Prelude
import MicroHs.Expr
import MicroHs.Ident

-- Operators resolution
--  Input:  A sequence of binary infix operators, prefix operators, and operands
--   b - infix op
--   p - prefix op
--   o - any op
--   e - operand
--  Three inputs: stack of operand, stack of operators, input sequence
--    [e          ] [       ] [           ]
--         ->  done
--    [e2, e1, ...] [by, ...] [bx, ...]
--         ->  ambig,                                     if prec bx == prec by && (assoc bx /= assoc by || assoc bx == None)
--         ->  [e1 `by` e2, ...] [        ...] [bx, ...], if prec bx < prec by || prec bx == prec by && assoc bx == Left
--         ->  [e2, e1,     ...] [bx, by, ...] [    ...], otherwise
--    [...]         []        [bx, e, ...]
--         ->  [e, ...         ] [bx         ] [        ...]
--    [...]         [...]     [e, ...]
--         ->  [e, ...         ] [        ...] [        ...]
--
--
--    [e2, e1, ...] [py, ...] [bx, ...]
--         ->  error,                                    if prec bx == prec py && assoc bx /= assoc py
--         ->  [py e2, e1, ...] [...        ] [bx, ...], if prec bx < prec py || prec bx == prec py
--         ->  [   e2, e1, ...] [bx, py, ...] [    ...], otherwise
--
--    [e2, e1, ...] [by, ...] [px,  ...]
--         ->  error                                     if prec px <= prec by
--         ->  [   e2, e1, ...] [px, by, ...] [    ...], otherwise
--    [e2, e1, ...] [py, ...] [px,  ...]
--         ->  [   e2, e1, ...] [px, py, ...] [    ...], otherwise

data FixInput = Rator Bool Expr Fixity | Rand Expr   -- True=infix, False=prefix
  deriving (Show)

eNeg :: SLoc -> Expr
eNeg loc = EVar (mkIdentSLoc loc "negate")

negFixity :: Fixity
negFixity = (AssocLeft, 6)

resolveFixity :: Expr -> [((Expr, Fixity), Expr)] -> Expr
resolveFixity ae oes =
  let inps = expr ae ++ concatMap opexpr oes
      expr (ESectR i e) | i == mkIdent "-" = Rator False (eNeg (getSLoc i)) negFixity : expr e
      expr e = [Rand e]
      opexpr ((f, fx), e) = Rator True f fx : expr e

      oper (Rator False f _) (e:es) = EApp f e : es
      oper (Rator True  f _) (e2:e1:es) = EApp (EApp f e1) e2 : es
      oper _ _ = undefined

      prec (Rator _ _ (_, p)) = p
      prec _ = undefined
      assoc (Rator _ _ (a, _)) = a
      assoc _ = undefined

      resolve [e] []             [] = e
      resolve _   []             [] = undefined
      resolve es  []             (ox@(Rator _    _    _) : is) = resolve          es  [ox] is
      resolve es     (oy:os)     []                            = resolve (oper oy es)  os  []
      resolve es aos             (    Rand e             : is) = resolve       (e:es) aos  is
      resolve es aos@(oy:os) ais@(ox@(Rator True func _) : is)
        | prec ox == prec oy && (assoc ox /= assoc oy || assoc ox == AssocNone) =
          errorMessage (getSLoc func) "ambiguous operator expression"
        | prec ox <  prec oy || prec ox == prec oy && assoc oy == AssocLeft =
          resolve (oper oy es) os ais
        | otherwise =
          resolve es (ox:aos) is
      resolve es aos@(oy:_)      (ox@(Rator False func _) : is)
        | prec ox <= prec oy =
          errorMessage (getSLoc func) "bad prefix expression"
        | otherwise =
          resolve es (ox:aos) is
  in  resolve [] [] inps
