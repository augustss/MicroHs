-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-do-bind #-}
module MicroHs.Parse(P, pTop, pTopModule, parseDie, parse, pExprTop, keywords, dotDotIdent) where
import Prelude(); import MHSPrelude
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Text.ParserComb as P
import MicroHs.Lex
import MicroHs.Expr hiding (getSLoc)
import qualified MicroHs.Expr as E
import MicroHs.Ident
--import Debug.Trace

type P a = Prsr LexState Token a

parseDie :: forall a . (Show a) =>
            P a -> FilePath -> String -> a
parseDie p fn file =
  case parse p fn file of
    Left msg -> error msg
    Right a -> a

parse :: forall a . (Show a) =>
         P a -> FilePath -> String -> Either String a
parse p fn file =
  let { ts = lexTopLS fn file } in
  case runPrsr p ts of
    Left lf -> Left $ formatFailed lf
    Right [a] -> Right a
    Right as -> Left $ "Ambiguous:"
                       ++ unlines (map show as)
guardM :: P a -> (a -> Bool) -> P a
guardM ma p = do a <- ma; guard (p a); pure a

getSLoc :: P SLoc
getSLoc = do
  t <- nextToken
  pure (tokensLoc [t])

eof :: P ()
eof = do
  t <- nextToken
  case t of
    TEnd _ -> pure ()
    _      -> fail "eof"

pTop :: P EModule
pTop = (pModule <|< pModuleEmpty) <* eof

pTopModule :: P EModule
pTopModule = pModule <* eof

pExprTop :: P Expr
pExprTop = pBraces pExpr <* eof

pModule :: P EModule
pModule = do
  pKeyword "module"
  mn <- pUQIdentA
  exps <- (pSpec '(' *> esepEndBy pExportItem (pSpec ',') <* pSpec ')')
      <|< pure [ExpModule mn]
  pKeyword "where"
  defs <- pBlock pDef
  pure $ EModule mn exps defs

pModuleEmpty :: P EModule
pModuleEmpty = do
  defs <- pBlock pDef
  --let loc = getSLoc defs
  pure $ EModule (mkIdent "Main") [ExpValue $ mkIdent "main"] defs

-- Possibly qualified alphanumeric identifier
pQIdent :: P Ident
pQIdent = do
  let
    is (TIdent loc qs s) | isAlpha_ (head s) = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "QIdent" is

-- Upper case, unqualified, alphanumeric identifier
pUIdentA :: P Ident
pUIdentA = do
  let
    is (TIdent loc [] s) | isUpper (head s) = Just (mkIdentSLoc loc s)
    is _ = Nothing
  satisfyM "UIdent" is

-- Upper case, unqualified, identifier
pUIdent :: P Ident
pUIdent =
      pUIdentA
  <|< pUIdentSpecial

-- Upper case, unqualified, identifier or symbol
pUIdentSym :: P Ident
pUIdentSym = pUIdent <|< pParens pUSymOper

-- Special "identifiers": [] (,) ...
pUIdentSpecial :: P Ident
pUIdentSpecial = do
  loc <- getSLoc
  let
    mk = mkIdentSLoc loc
  (mk . map (const ',') <$> (pSpec '(' *> esome (pSpec ',') <* pSpec ')'))
    <|< (mk "[]" <$ (pSpec '[' *> pSpec ']'))  -- Allow [] as a constructor name

-- Upper case, possibly qualified, alphanumeric identifier
pUQIdentA :: P Ident
pUQIdentA = do
  let
    is (TIdent loc qs s) | isUpper (head s) = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "UQIdent" is

-- Upper case, possibly qualified, identifier
pUQIdent :: P Ident
pUQIdent =
      pUQIdentA
  <|< pUIdentSpecial

-- Lower case, unqualified identifier
pLIdent :: P Ident
pLIdent = do
  let
    is (TIdent loc [] s) | isLower_ (head s) && not (elem s keywords) = Just (mkIdentSLoc loc s)
    is _ = Nothing
  satisfyM "LIdent" is

-- Lower case, possibly qualified identifier
pLQIdent :: P Ident
pLQIdent = do
  let
    is (TIdent loc qs s) | isLower_ (head s) && not (elem s keywords) = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "LQIdent" is

-- Type names can be any operator
pTypeIdentSym :: P Ident
pTypeIdentSym = pUIdent <|< pParens pSymOper

keywords :: [String]
keywords =
  ["case", "class", "data", "default", "deriving", "do", "else", "forall", "foreign", "if",
   "import", "in", "infix", "infixl", "infixr", "instance",
   "let", "module", "newtype", "of", "pattern", "primitive", "then", "type", "where"]

pSpec :: Char -> P ()
pSpec c = () <$ satisfy (showToken $ TSpec (SLoc "" 0 0) c) is
  where
    is (TSpec _ d) = c == d
    is _ = False

pSymbol :: String -> P ()
pSymbol sym = () <$ satisfy sym is
  where
    is (TIdent _ [] s) = s == sym
    is _ = False

pOper :: P Ident
pOper = pQSymOper <|< (pSpec '`' *> pQIdent <* pSpec '`')

pUOper :: P Ident
pUOper = pUQSymOper <|< (pSpec '`' *> pUQIdent <* pSpec '`')

pQSymOper :: P Ident
pQSymOper = do
  let
    is (TIdent loc qs s) | not (isAlpha_ (head s)) && not (elem s reservedOps) = Just (qualName loc qs s)
    is (TSpec  loc '!') = Just (mkIdentSLoc loc "!")
    is (TSpec  loc '~') = Just (mkIdentSLoc loc "~")
    is _ = Nothing
  satisfyM "QSymOper" is

pSymOper :: P Ident
pSymOper = do
  let
    is (TIdent loc [] s) | not (isAlpha_ (head s)) && not (elem s reservedOps) = Just (mkIdentSLoc loc s)
    is (TSpec  loc '!') = Just (mkIdentSLoc loc "!")
    is _ = Nothing
  satisfyM "SymOper" is

pUQSymOper :: P Ident
pUQSymOper = guardM pQSymOper isUOper

isUOper :: Ident -> Bool
isUOper = (== ':') . headIdent

pUSymOper :: P Ident
pUSymOper = guardM pSymOper isUOper

pLQSymOper :: P Ident
pLQSymOper = guardM pQSymOper (not . isUOper)

-- Allow -> as well
pLQSymOperArr :: P Ident
pLQSymOperArr = pLQSymOper <|< pQArrow

-- Parse ->, possibly qualified
pQArrow :: P Ident
pQArrow = do
  let
    is (TIdent loc qs s@"->") = Just (qualName loc qs s)
    is (TIdent loc qs s@"\x2192") = Just (qualName loc qs s)
    is _ = Nothing
  satisfyM "->" is

pLSymOper :: P Ident
pLSymOper = guardM pSymOper (not . isUOper)

reservedOps :: [String]
reservedOps = ["::", "<-", "..", "->",
               "\x2237", "\x2192"] -- :: and ->

pUQIdentSym :: P Ident
pUQIdentSym = pUQIdent <|< pParens pUQSymOper

pLQIdentSym :: P Ident
pLQIdentSym = pLQIdent <|< pParens pLQSymOperArr

pLIdentSym :: P Ident
pLIdentSym = pLIdent <|< pParens pLSymOper

pParens :: forall a . P a -> P a
pParens p = pSpec '(' *> p <* pSpec ')'

pLit :: P Expr
pLit = do
  let
    is (TString loc s) = Just (ELit loc (LStr s))
    is (TChar   loc a) = Just (ELit loc (LChar a))
    is (TInt    loc i) = Just (ELit loc (LInteger i))
    is (TRat    loc d) = Just (ELit loc (LRat d))
    is _ = Nothing
  satisfyM "literal" is

pNumLit :: P Expr
pNumLit = guardM pLit isNum
  where isNum (ELit _ (LInteger _)) = True
        isNum (ELit _ (LRat _)) = True
        isNum _ = False

pString :: P String
pString = satisfyM "string" is
  where
    is (TString _ s) = Just s
    is _ = Nothing

---------------

pExportItem :: P ExportItem
pExportItem =
      ExpModule   <$> (pKeyword "module" *> pUQIdent)
  <|< ExpTypeSome <$> pUQIdentSym <*> pParens pConList
  <|< ExpTypeSome <$> pUQIdentSym <*> pure []
  <|< ExpValue    <$> pLQIdentSym
  <|< ExpValue    <$> (pKeyword "pattern" *> pUQIdentSym)
  <|< ExpTypeSome <$> (pKeyword "type" *> pLQIdentSym) <*> pure []
  <|< ExpDefault  <$> (pKeyword "default" *> pUQIdentSym)

pKeyword :: String -> P ()
pKeyword kw = () <$ satisfy kw is
  where
    is (TIdent _ [] s) = kw == s
    is _ = False

pPragma :: String -> P ()
pPragma kw = () <$ satisfy kw is
  where
    is (TPragma _ s) = kw == s
    is _ = False

pBraces :: forall a . P a -> P a
pBraces p =
  do
    pSpec '{'
    as <- p
    pSpec '}'
    pure as
 <|<
  do
    pSpec '<'          -- synthetic '{' (i.e., layout)
    as <- p
    -- If we are at a '>' token (i.e., synthetic '}') then
    -- all is well, if not then there is a parse error and we try
    -- recovering by popping they layout stack.
    -- This implements the Note 5 rule from Section 10.3 in
    -- the Haskell report.
    t <- nextToken
    case t of
      TSpec _ '>' -> pSpec '>'
      _           -> mapTokenState popLayout
    pure as

pBlock :: forall a . P a -> P [a]
pBlock p = pBraces body
  where body = esepBy p (esome (pSpec ';')) <* eoptional (pSpec ';')


pDef :: P EDef
pDef =
      pBind'      -- Fcn, Sign, PatBind, Infix
  <|< uncurry Data <$> (pKeyword "data"     *> pData) <*> pDeriving
  <|< Newtype      <$> (pKeyword "newtype"  *> pLHS) <*> (pSpec '=' *> (Constr [] [] <$> pUIdentSym <*> pField)) <*> pDeriving
  <|< Type         <$> (pKeyword "type"     *> pLHS) <*> (pSpec '=' *> pType)
  <|< Import       <$> (pKeyword "import"   *> pImportSpec)
  <|< ForImp       <$> (pKeyword "foreign"  *> pKeyword "import" *> (pKeyword "ccall" <|> pKeyword "capi")
                        *> eoptional (pKeyword "unsafe") *> eoptional pString) <*> pLIdent <*> (dcolon *> pType)
  <|< Class        <$> (pKeyword "class"    *> pContext) <*> pLHS <*> pFunDeps     <*> pWhere pClsBind
  <|< Instance     <$> (pKeyword "instance" *> pType) <*> pWhere pInstBind
  <|< Default      <$> (pKeyword "default"  *> eoptional clsSym) <*> pParens (esepBy pType (pSpec ','))
  <|< KindSign     <$> (pKeyword "type"     *> pTypeIdentSym) <*> (dcolon *> pKind)
  <|< mkPattern    <$> (pKeyword "pattern"  *> pPatSyn)
  <|< Sign         <$> (pKeyword "pattern"  *> (esepBy1 pUIdentSym (pSpec ',')) <* dcolon) <*> pType
  <|< Deriving     <$> (pKeyword "deriving" *> pKeyword "instance" *> pType)
  <|< noop         <$  (pKeyword "type"     <* pKeyword "role" <* pTypeIdentSym <*
                                               (pKeyword "nominal" <|> pKeyword "phantom" <|> pKeyword "representational"))
  where
    pFunDeps = (pSpec '|' *> esepBy1 pFunDep (pSpec ',')) <|< pure []
    pFunDep = (,) <$> esome pLIdent <*> (pSRArrow *> esome pLIdent)
    pField = guardM pFields ((== 1) . either length length)

    clsSym = do s <- pUIdentSym; guard (unIdent s /= "()"); return s

    mkPattern (lhs, pat, meqn) = Pattern lhs pat meqn
    noop = Infix (AssocLeft, 0) []        -- harmless definition

pPatSyn :: P (LHS, EPat, Maybe [Eqn])
pPatSyn = do
  lhs@(i, vs) <- pLHS
  ( do pSpec '=';
       p <- pPat
       guard (isExp p)
       let eqn = eEqn (map (EVar . idKindIdent) vs) p
       pure (lhs, p, Just [eqn])
   ) <|> (
    do pSymbol "<-"
       p <- pPat
       meqns <- optional (pKeyword "where" *> pBraces (pEqnsU i))
       pure (lhs, p, fmap snd meqns)
   )

dcolon :: P ()
dcolon = pSymbol "::" <|< pSymbol "\x2237"

-- Is a pattern also an expression?
isExp :: Expr -> Bool
isExp (EVar _) = True
isExp (EListish (LList es)) = all isExp es
isExp (ETuple es) = all isExp es
isExp (EApp e1 e2) = isExp e1 && isExp e2
isExp (ELit _ _) = True
isExp _ = False

pData :: P (LHS, [Constr])
pData = do
  lhs <- pLHS
  let pConstrs = pSpec '=' *> esepBy1 pConstr (pSpec '|')
  ((,) lhs <$> pConstrs)
   <|< pGADT lhs
   <|< pure (lhs, [])

pGADT :: LHS -> P (LHS, [Constr])
pGADT (n, vks) = do
  let f (IdKind i k) = IdKind (addIdentSuffix i "$") k
      lhs = (n, map f vks)
  pKeyword "where"
  gs <- pBlock pGADTconstr
  pure (lhs, map (dsGADT lhs) gs)

pGADTconstr :: P (Ident, [IdKind], [EConstraint], [SType], EType)
pGADTconstr = do
  cn <- pUIdentSym
  dcolon
  es <- pForall
  ctx <- pContext
  args <- emany (pSTypeApp <* pSymbol "->")
  res <- pType
  pure (cn, es, ctx, args, res)

dsGADT :: LHS -> (Ident, [IdKind], [EConstraint], [SType], EType) -> Constr
dsGADT (tnm, vks) (cnm, es, ctx, stys, rty) =
  case getAppM rty of
    Just (tnm', ts) | tnm == tnm' && length vks == length ts -> Constr es' ctx' cnm (Left stys)
      where es' = if null es then map (\ i -> IdKind i (EVar dummyIdent)) (freeTyVars (rty : map snd stys)) else es
            ctx' = zipWith (\ (IdKind i _) t -> eq (EVar i) t) vks ts ++ ctx
            eq t1 t2 = EApp (EApp (EVar (mkIdentSLoc (E.getSLoc t1) "~")) t1) t2
    _ -> errorMessage (E.getSLoc rty) $ "Bad GADT result type" ++ show (rty, tnm, vks)

pDeriving :: P [EConstraint]
pDeriving = pKeyword "deriving" *> pDer <|< pure []
  where pDer =     pParens (esepBy pType (pSpec ','))
               <|< ((:[]) <$> pType)

-- List has 0 or 1 elements
pContext :: P [EConstraint]
pContext = (pCtx <* pDRArrow) <|< pure []
  where
    pCtx =     ((:[]) <$> pTypeApp)
           <|> (eq <$> pTypeArg <*> pTilde <*> pTypeArg)   -- A hack to allow   a~b => ...
    eq t1 i t2 = [eAppI2 i t1 t2]
    pTilde = do i <- pQSymOper; guard (i == mkIdent "~"); return i

pDRArrow :: P ()
pDRArrow = pSymbol "=>" <|< pSymbol "\x21d2"

pSRArrow :: P ()
pSRArrow = pSymbol "->" <|< pSymbol "\x2192"

pSLArrow :: P ()
pSLArrow = pSymbol "<-" <|< pSymbol "\x2190"

pConstr :: P Constr
pConstr = (Constr <$> pForall <*> pContext <*> pUIdentSym <*> pFields)
      <|> ((\ vs ct t1 c t2 -> Constr vs ct c (Left [t1, t2])) <$>        -- <|> needed
            pForall <*> pContext <*> pSTypeApp <*> pUSymOper <*> pSTypeApp)


pFields :: P (Either [SType] [(Ident, SType)])
pFields = Left  <$> emany pSAType
      -- The <|> is needed because 'emany' can be empty.
      <|> Right <$> (pSpec '{' *> (concatMap flat <$> esepBy ((,) <$> (esepBy1 pLIdentSym (pSpec ',') <* dcolon) <*> pSType) (pSpec ',') <* pSpec '}'))
  where flat (is, t) = [ (i, t) | i <- is ]

-- XXX This is a mess.
pSAType :: P (Bool, EType)
pSAType = (,) <$> pStrict <*> pAType
pSType :: P (Bool, EType)
pSType  = (,) <$> pStrict <*> pType
pSTypeApp :: P (Bool, EType)
pSTypeApp  = do
  s <- pStrict
  t <- if s then pAType else pTypeApp
  pure (s, t)
pStrict :: P Bool
pStrict = (True <$ pSpec '!') <|< pure False

pLHS :: P LHS
pLHS = (,) <$> pTypeIdentSym <*> emany pIdKind
    <|< (\ a c b -> (c, [a,b])) <$> pIdKind <*> pSymOper <*> pIdKind

pImportSpec :: P ImportSpec
pImportSpec =
  let
    pSource = (ImpBoot <$ pPragma "SOURCE") <|< pure ImpNormal
    pQual = True <$ pKeyword "qualified"
    -- the 'qualified' can occur before or after the module name
    pQId =      ((,) <$> pQual <*> pUQIdentA)
            <|< ((\ a b -> (b,a)) <$> pUQIdentA <*> (pQual <|< pure False))
    imp a (b, c) = ImportSpec a b c
  in  imp <$> pSource <*> pQId <*> eoptional (pKeyword "as" *> pUQIdent) <*>
              eoptional ((,) <$> ((True <$ pKeyword "hiding") <|< pure False) <*> pParens (esepEndBy pImportItem (pSpec ',')))

pImportItem :: P ImportItem
pImportItem =
      impType     <$> pUQIdentSym <*> pParens pConList
  <|< ImpTypeSome <$> pUQIdentSym <*> pure []
  <|< ImpValue    <$> pLQIdentSym
  <|< ImpValue    <$> (pKeyword "pattern" *> pUQIdentSym)
  <|< ImpTypeSome <$> (pKeyword "type" *> pLQIdentSym) <*> pure []
  where impType i [d] | d == dotDotIdent = ImpTypeAll  i
        impType i is                     = ImpTypeSome i is

pConList :: P [Ident]
pConList = esepBy (pDotDot <|< pQIdent <|< pUIdentSpecial <|< pParens pSymOper) (pSpec ',')
  where pDotDot = dotDotIdent <$ pSymbol ".."

dotDotIdent :: Ident
dotDotIdent = mkIdent ".."

--------
-- Types

pIdKind :: P IdKind
pIdKind =
      ((\ i -> IdKind i (EVar dummyIdent)) <$> pLIdentSym)          -- dummyIdent indicates that we have no kind info
  <|< pParens (IdKind <$> pLIdentSym <*> (dcolon *> pKind))

pKind :: P EKind
pKind = pType

--
-- Partial copy of pExpr, but that includes '->'.
-- Including '->' in pExprOp interacts poorly with '->'
-- in lambda and 'case'.
pType :: P EType
pType = do
  vs <- pForall
  t <- pTypeOp
  pure $ if null vs then t else EForall True vs t

pForall :: P [IdKind]
pForall = (forallKW *> esome pIdKind <* pSymbol ".") <|< pure []
  where forallKW = pKeyword "forall" <|< pSymbol "\x2200"

pTypeOp :: P EType
pTypeOp = pOperators pTypeOper pTypeArg

pTypeOper :: P Ident
pTypeOper = pOper <|< (mkIdent "->" <$ pSRArrow) <|< (mkIdent "=>" <$ pDRArrow)

pTypeArg :: P EType
pTypeArg = pTypeApp

pTypeApp :: P EType
pTypeApp = do
  f <- pAType
  as <- emany pAType
  pure $ foldl EApp f as

pAType :: P Expr
pAType =
      (EVar <$> pLQIdentSym)
  <|< (EVar <$> pUQIdentSym)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy pType (pSpec ',') <* pSpec ')'))
  <|< (EListish . LList . (:[]) <$> (pSpec '[' *> pType <* pSpec ']'))  -- Unlike expressions, only allow a single element.

-------------
-- Patterns

-- Sadly pattern and expression parsing cannot be joined because the
-- use of '->' in 'case' and lambda makes it weird.
-- Instead this is just a copy of some of the expression rules.
-- XXX This can probably be joined with pExpr again now that pType
-- is separate.
pAPat :: P EPat
pAPat =
      (do
         i <- pLIdentSym
         (EAt i <$> (pSpec '@' *> pAPat)) <|< pure (EVar i)
      )
  <|< (evar <$> pUQIdentSym <*> optional pUpdate)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy pPat (pSpec ',') <* pSpec ')'))
  <|< (EListish . LList <$> (pSpec '[' *> esepBy1 pPat (pSpec ',') <* pSpec ']'))
  <|< (EViewPat <$> (pSpec '(' *> pExpr) <*> (pSRArrow *> pPat <* pSpec ')'))
  <|< (ELazy True  <$> (pSpec '~' *> pAPat))
  <|< (ELazy False <$> (pSpec '!' *> pAPat))
  <|< (EOr <$> (pSpec '(' *> esepBy1 pPat (pSpec ';') <* pSpec ')'))  -- if there is a single pattern it will be matched by the tuple case
  where evar v Nothing = EVar v
        evar v (Just upd) = EUpdate (EVar v) upd

pPat :: P EPat
pPat = pPatOp
  -- This is where view patterns belong, but it's too slow
  --  <|> (EViewPat <$> pExpr <*> (pSRArrow *> pPatApp))

pPatOp :: P EPat
pPatOp = pOperators pUOper pPatArg

pPatArg :: P EPat
pPatArg = (pSymbol "-" *> (ENegApp <$> pNumLit)) <|< pPatApp

pPatApp :: P EPat
pPatApp = do
  f <- pAPat
  as <- emany pAPat
  guard (null as || isPConApp f)
  pure $ foldl EApp f as

pPatNotVar :: P EPat
pPatNotVar = guardM pPat isPConApp

-------------

-- Regular function definition
pEqns :: P (Ident, [Eqn])
pEqns = pEqns' pLIdentSym pLOper (\ _ _ -> True)
  where pLOper = guardM pOper (not . isConIdent)

-- Pattern synonym function; must have name i.
pEqnsU :: Ident -> P (Ident, [Eqn])
pEqnsU i = pEqns' pUIdentSym pUOper (\ n _ -> i == n)

-- pEqns' is used to parse oridinary function definitions as well
-- as the 'constructor' of pattern synonyms, which has an upper case identifier.
pEqns' :: P Ident -> P Ident -> (Ident -> Int -> Bool) -> P (Ident, [Eqn])
pEqns' ident oper test = do
  (name, eqn@(Eqn ps alts)) <- pEqn ident oper test
  case (ps, alts) of
    ([], EAlts [_] []) ->
      -- don't collect equations when of the form 'i = e'
      pure (name, [eqn])
    _ -> do
      neqns <- emany (pSpec ';' *> pEqn ident oper (\ n l -> n == name && l == length ps))
      pure (name, eqn : map snd neqns)

pEqn :: P Ident -> P Ident -> (Ident -> Int -> Bool) -> P (Ident, Eqn)
pEqn ident oper test = do
  (name, pats) <- pEqnLHS ident oper
  alts <- pAlts (pSpec '=')
  guard (test name (length pats))
  pure (name, Eqn pats alts)

pEqnLHS :: P Ident -> P Ident -> P (Ident, [EPat])
pEqnLHS ident oper =
  ((,) <$> ident <*> emany pAPat)
  <|>   -- XXX this <|> causes a slowdown, but is necessary
  pOpLHS
  <|<
  ((\ (i, ps1) ps2 -> (i, ps1 ++ ps2)) <$> pParens pOpLHS <*> emany pAPat)
  where
    pOpLHS = (\ p1 i p2 -> (i, [p1,p2])) <$> pPatApp <*> oper <*> pPatApp

pAlts :: P () -> P EAlts
pAlts sep = do
  alts <- pAltsL sep
  bs <- pWhere pBind
  pure (EAlts alts bs)
  
pAltsL :: P () -> P [EAlt]
pAltsL sep =
      esome (pAlt sep)
  <|< ((\ e -> [([], e)]) <$> (sep *> pExpr))

pAlt :: P () -> P EAlt
pAlt sep = (,) <$> (pSpec '|' *> esepBy1 pStmt (pSpec ',')) <*> (sep *> pExpr)

pWhere :: P EBind -> P [EBind]
pWhere pb =
      (pKeyword "where" *> pBlock pb)
  <|< pure []

-------------
-- Statements

pStmt :: P EStmt
pStmt =
      (SBind <$> (pPat <* pSLArrow) <*> pExpr)
  <|< (SLet  <$> (pKeyword "let" *> pBlock pBind))
  <|< (SThen <$> pExpr)

-------------
-- Expressions

pExpr :: P Expr
pExpr = pExprOp

pExprArg :: P Expr
pExprArg = pExprApp <|< pLam <|< pCase <|< pLet <|< pIf <|< pDo

pExprApp :: P Expr
pExprApp = do
  f <- pAExpr
  as <- emany pAExpr
  pure $ foldl EApp f as

pLam :: P Expr
pLam = do
  loc <- getSLoc
  pSpec '\\' *>
    (    eLamWithSLoc loc <$> esome pAPat <*> (pSRArrow *> pExpr)
     <|< eLamCase loc <$> (pKeyword "case" *> pBlock pCaseArm)
    )

eLamCase :: SLoc -> [ECaseArm] -> Expr
eLamCase loc as = ELam loc [ Eqn [p] a | (p, a) <- as ]

pCase :: P Expr
pCase = ECase <$> (pKeyword "case" *> pExpr) <*> (pKeyword "of" *> pBlock pCaseArm)

pCaseArm :: P ECaseArm
pCaseArm = (,) <$> pPat <*> pAlts pSRArrow

pLet :: P Expr
pLet = ELet <$> (pKeyword "let" *> pBlock pBind) <*> (pKeyword "in" *> pExpr)

pDo :: P Expr
pDo = do
  q <- (Just <$> pQualDo) <|< (Nothing <$ pKeyword "do")
  ss <- pBlock pStmt
  guard (not (null ss))
  pure (EDo q ss)

pIf :: P Expr
pIf = EIf <$> (pKeyword "if" *> pExpr) <*>
              (eoptional (pSpec ';') *> pKeyword "then" *> pExpr) <*>
              (eoptional (pSpec ';') *> pKeyword "else" *> pExpr)
  <|< EMultiIf <$> (EAlts <$> (pKeyword "if" *> pBlock (pAlt (pSymbol "->"))) <*> pure [])

pQualDo :: P Ident
pQualDo = do
  let
    is (TIdent loc qs@(_:_) "do") = Just (mkIdentSLoc loc (intercalate "." qs))
    is _ = Nothing
  satisfyM "QualDo" is

pOperComma :: P Ident
pOperComma = pOper <|< pComma
  where
    pComma = mkIdentSLoc <$> getSLoc <*> ("," <$ pSpec ',')

-- No right section for '-'.
pOperCommaNoMinus :: P Ident
pOperCommaNoMinus = guardM pOperComma (/= mkIdent "-")

-- XXX combine pUpdate and pSelects
pAExpr :: P Expr
pAExpr = do
  ee <- pAExpr'
  us <- many pUpdate
  ss <- many pSelect
  let sel e | null ss = e
            | otherwise = EApp (ESelect ss) e
  pure $ sel (foldl EUpdate ee us)

pUpdate :: P [EField]
pUpdate = pSpec '{' *> esepBy pEField (pSpec ',') <* pSpec '}'
  where
    pEField = do
      fs <- (:) <$> pLIdentSym <*> many pSelect
      EField fs <$> (pSpec '=' *> pExpr) <|< pure (EFieldPun fs)
     <|<
      (EFieldWild <$ pSymbol "..")

pSelect :: P Ident
pSelect = pSpec '.' *> pLIdent

pAExpr' :: P Expr
pAExpr' = (
      (EVar   <$> pLQIdentSym)
  <|< (EVar   <$> pUQIdentSym)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy pExpr (pSpec ',') <* pSpec ')'))
  <|< EListish <$> (pSpec '[' *> pListish <* pSpec ']')
  <|< (ESectL <$> (pSpec '(' *> pExprOp) <*> (pOperComma <* pSpec ')'))
  <|< (ESectR <$> (pSpec '(' *> pOperCommaNoMinus) <*> (pExprOp <* pSpec ')'))
  <|< (ESelect <$> (pSpec '(' *> esome pSelect <* pSpec ')'))
  <|< (ELit noSLoc . LPrim <$> (pKeyword "primitive" *> pString))
  <|< (ETypeArg <$> (pSpec '@' *> pAType))
  )
  -- This weirdly slows down parsing
  -- <?> "aexpr"

pListish :: P Listish
pListish = do
  e1 <- pExpr
  let
    pMore = do
      e2 <- pExpr
      ((\ es -> LList (e1:e2:es)) <$> esome (pSpec ',' *> pExpr))
       <|< (LFromThenTo e1 e2 <$> (pSymbol ".." *> pExpr))
       <|< (LFromThen e1 e2 <$ pSymbol "..")
       <|< pure (LList [e1,e2])
  (pSpec ',' *> pMore)
   <|< (LCompr e1 <$> (pSpec '|' *> esepBy1 pStmt (pSpec ',')))
   <|< (LFromTo e1 <$> (pSymbol ".." *> pExpr))
   <|< (LFrom e1 <$ pSymbol "..")
   <|< pure (LList [e1])

pExprOp :: P Expr
pExprOp = pOperators pOper pExprArgNeg

pExprArgNeg :: P Expr
pExprArgNeg = (pSymbol "-" *> (ENegApp <$> pExprArg)) <|< pExprArg

pOperators :: P Ident -> P Expr -> P Expr
pOperators oper one = do
  r <- pOperators' oper one
  mt <- eoptional (dcolon *> pType)
  pure $ maybe r (ESign r) mt

pOperators' :: P Ident -> P Expr -> P Expr
pOperators' oper one = eOper <$> one <*> emany ((,) <$> oper <*> one)
  where eOper e [] | notNeg e = e
        eOper e ies = EOper e ies
        notNeg (ENegApp _) = False
        notNeg _ = True

-------------
-- Bindings

-- Bindings allowed in a let
pBind :: P EBind
pBind =
      pBind'
  <|< PatBind     <$> pPatNotVar <*> ((pSpec '=' *> pExpr)
                                      <|<
                                      (EMultiIf <$> pAlts (pSpec '=')))

-- Bindings allowed in top level, let, class
pBind' :: P EBind
pBind' =
      uncurry Fcn <$> pEqns
  <|< Sign        <$> ((esepBy1 pLIdentSym (pSpec ',')) <* dcolon) <*> pType
  <|< Infix       <$> ((,) <$> pAssoc <*> pPrec) <*> esepBy1 pTypeOper (pSpec ',')
  where
    pAssoc = (AssocLeft <$ pKeyword "infixl") <|< (AssocRight <$ pKeyword "infixr") <|< (AssocNone <$ pKeyword "infix")
    dig (TInt _ ii) | 0 <= i && i <= 9 = Just i  where i = fromInteger ii
    dig _ = Nothing
    pPrec = satisfyM "digit" dig <|< pure 9

-- Bindings allowed in a class definition
pClsBind :: P EBind
pClsBind =
      pBind'
  <|< DfltSign    <$> (pKeyword "default" *> pLIdentSym <* dcolon) <*> pType

-- Bindings allowed in an instance definition
pInstBind :: P EBind
pInstBind = 
      uncurry Fcn <$> pEqns
-- no InstanceSig yet  <|< Sign        <$> (pLIdentSym <* dcolon) <*> pType

-------------

eTuple :: [Expr] -> Expr
eTuple [e] = EParen e
eTuple es = ETuple es

isAlpha_ :: Char -> Bool
isAlpha_ c = isLower_ c || isUpper c

qualName :: SLoc -> [String] -> String -> Ident
qualName loc qs s = mkIdentSLoc loc (intercalate "." (qs ++ [s]))

-------------

formatFailed :: LastFail Token -> String
formatFailed (LastFail _ ts msgs) =
  let
    sloc = tokensLoc ts
  in
    showSLoc sloc ++ ":\n"
      ++ "  found:    " ++ head (map showToken ts ++ ["EOF"]) ++ "\n"
      ++ "  expected: " ++ unwords (nub msgs)
