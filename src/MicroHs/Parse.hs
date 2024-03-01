-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-do-bind #-}
module MicroHs.Parse(P, pTop, pTopModule, parseDie, parse, pExprTop) where
import Prelude
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Text.ParserComb as P
import MicroHs.Lex
import MicroHs.Expr
import MicroHs.Ident
--import Debug.Trace

type P a = Prsr FilePath LexState Token a

getFileName :: P FilePath
getFileName = get

parseDie :: forall a . (Show a) =>
            P a -> FilePath -> String -> a
parseDie p fn file =
  case parse p fn file of
    Left msg -> error msg
    Right a -> a

parse :: forall a . (Show a) =>
         P a -> FilePath -> String -> Either String a
parse p fn file =
  let { ts = lexTopLS file } in
  case runPrsr fn p ts of
    Left lf -> Left $ formatFailed fn (tmRawTokens ts) lf
    Right [(a, _)] -> Right a
    Right as -> Left $ "Ambiguous:"
                       ++ unlines (map (show . fst) as)

getLoc :: P Loc
getLoc = do
  t <- nextToken
  pure (tokensLoc [t])

eof :: P ()
eof = do
  t <- nextToken
  case t of
    TEnd -> pure ()
    _    -> fail "eof"

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
  fn <- getFileName
  let
    is (TIdent loc qs s) | isAlpha_ (head s) = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "QIdent" is

-- Upper case, unqualified, alphanumeric identifier
pUIdentA :: P Ident
pUIdentA = do
  fn <- getFileName
  let
    is (TIdent loc [] s) | isUpper (head s) = Just (mkIdentLoc fn loc s)
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

-- Special "identifiers": () [] (,) ...
pUIdentSpecial :: P Ident
pUIdentSpecial = do
  fn <- getFileName
  loc <- getLoc
  let
    mk = mkIdentLoc fn loc
  
  (mk . map (const ',') <$> (pSpec '(' *> esome (pSpec ',') <* pSpec ')'))
    <|< (mk "()" <$ (pSpec '(' *> pSpec ')'))  -- Allow () as a constructor name
    <|< (mk "[]" <$ (pSpec '[' *> pSpec ']'))  -- Allow [] as a constructor name

-- Upper case, possibly qualified, alphanumeric identifier
pUQIdentA :: P Ident
pUQIdentA = do
  fn <- getFileName
  let
    is (TIdent loc qs s) | isUpper (head s) = Just (qualName fn loc qs s)
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
  fn <- getFileName
  let
    is (TIdent loc [] s) | isLower_ (head s) && not (elem s keywords) = Just (mkIdentLoc fn loc s)
    is _ = Nothing
  satisfyM "LIdent" is

-- Lower case, possibly qualified identifier
pLQIdent :: P Ident
pLQIdent = do
  fn <- getFileName
  let
    is (TIdent loc qs s) | isLower_ (head s) && not (elem s keywords) = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "LQIdent" is

-- Type names can be any operator
pTypeIdentSym :: P Ident
pTypeIdentSym = pUIdent <|< pParens pSymOper

keywords :: [String]
keywords =
  ["case", "class", "data", "default", "deriving", "do", "else", "forall", "foreign", "if",
   "import", "in", "infix", "infixl", "infixr", "instance",
   "let", "module", "newtype", "of", "primitive", "then", "type", "where"]

pSpec :: Char -> P ()
pSpec c = () <$ satisfy (showToken $ TSpec (0,0) c) is
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
  fn <- getFileName
  let
    is (TIdent loc qs s) | not (isAlpha_ (head s)) && not (elem s reservedOps) = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "QSymOper" is

pSymOper :: P Ident
pSymOper = do
  fn <- getFileName
  let
    is (TIdent loc [] s) | not (isAlpha_ (head s)) && not (elem s reservedOps) = Just (mkIdentLoc fn loc s)
    is _ = Nothing
  satisfyM "SymOper" is

pUQSymOper :: P Ident
pUQSymOper = do
  s <- pQSymOper
  guard (isUOper s)
  pure s

isUOper :: Ident -> Bool
isUOper = (== ':') . head . unIdent

pUSymOper :: P Ident
pUSymOper = do
  s <- pSymOper
  guard (isUOper s)
  pure s

pLQSymOper :: P Ident
pLQSymOper = do
  s <- pQSymOper
  guard (not (isUOper s))
  pure s

-- Allow -> as well
pLQSymOperArr :: P Ident
pLQSymOperArr = pLQSymOper <|< pQArrow

-- Parse ->, possibly qualified
pQArrow :: P Ident
pQArrow = do
  fn <- getFileName
  let
    is (TIdent loc qs s@"->") = Just (qualName fn loc qs s)
    is (TIdent loc qs s@"\x2192") = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "->" is

pLSymOper :: P Ident
pLSymOper = do
  s <- pSymOper
  guard (not (isUOper s))
  pure s

reservedOps :: [String]
reservedOps = ["=", "|", "::", "<-", "@", "..", "->",
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
  fn <- getFileName
  let
    is (TString (l, c) s) = Just (ELit (SLoc fn l c) (LStr s))
    is (TChar   (l, c) a) = Just (ELit (SLoc fn l c) (LChar a))
    is (TInt    (l, c) i) = Just (ELit (SLoc fn l c) (LInteger i))
    is (TRat    (l, c) d) = Just (ELit (SLoc fn l c) (LRat d))
    is _ = Nothing
  satisfyM "literal" is

pNumLit :: P Expr
pNumLit = do
  e <- pLit
  guard $ case e of { ELit _ (LInteger _) -> True; ELit _ (LRat _) -> True; _ -> False }
  return e

pString :: P String
pString = satisfyM "string" is
  where
    is (TString _ s) = Just s
    is _ = Nothing

---------------

pExportItem :: P ExportItem
pExportItem =
      ExpModule <$> (pKeyword "module" *> pUQIdent)
  <|< ExpTypeCon <$> (pUQIdentSym <* pSpec '(' <* pConList <* pSpec ')')
  <|< ExpType <$> pUQIdentSym
  <|< ExpValue <$> pLQIdentSym

pKeyword :: String -> P ()
pKeyword kw = () <$ satisfy kw is
  where
    is (TIdent _ [] s) = kw == s
    is _ = False

pBraces :: forall a . P a -> P a
pBraces p =
  do
    pSpec '{'
    as <- p
    pSpec '}'
    pure as
 <|>
  do
    pSpec '<'
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
      Data        <$> (pKeyword "data"    *> pLHS) <*> ((pSymbol "=" *> esepBy1 pConstr (pSymbol "|"))
                                                        <|< pure []) <*> pDeriving
  <|< Newtype     <$> (pKeyword "newtype" *> pLHS) <*> (pSymbol "=" *> (Constr [] [] <$> pUIdentSym <*> pField)) <*> pDeriving
  <|< Type        <$> (pKeyword "type"    *> pLHS) <*> (pSymbol "=" *> pType)
  <|< uncurry Fcn <$> pEqns
  <|< Sign        <$> ((esepBy1 pLIdentSym (pSpec ',')) <* dcolon) <*> pType
  <|< Import      <$> (pKeyword "import"  *> pImportSpec)
  <|< ForImp      <$> (pKeyword "foreign" *> pKeyword "import" *> pKeyword "ccall" *> eoptional pString) <*> pLIdent <*> (pSymbol "::" *> pType)
  <|< Infix       <$> ((,) <$> pAssoc <*> pPrec) <*> esepBy1 pTypeOper (pSpec ',')
  <|< Class       <$> (pKeyword "class"    *> pContext) <*> pLHS <*> pFunDeps     <*> pWhere pClsBind
  <|< Instance    <$> (pKeyword "instance" *> pType) <*> pWhere pClsBind
  <|< Default     <$> (pKeyword "default"  *> pParens (esepBy pType (pSpec ',')))
  <|< KindSign    <$> (pKeyword "type"    *> pTypeIdentSym) <*> (pSymbol "::" *> pKind)
  where
    pAssoc = (AssocLeft <$ pKeyword "infixl") <|< (AssocRight <$ pKeyword "infixr") <|< (AssocNone <$ pKeyword "infix")
    dig (TInt _ ii) | 0 <= i && i <= 9 = Just i  where i = fromInteger ii
    dig _ = Nothing
    pPrec = satisfyM "digit" dig

    pFunDeps = (pSymbol "|" *> esepBy1 pFunDep (pSpec ',')) <|< pure []
    pFunDep = (,) <$> esome pLIdent <*> (pSRArrow *> esome pLIdent)
    pField = do
      fs <- pFields
      guard $ either length length fs == 1
      pure fs
    dcolon = pSymbol "::" <|< pSymbol "\x2237"

pDeriving :: P [EConstraint]
pDeriving = pKeyword "deriving" *> pDer <|< pure []
  where pDer =     pParens (esepBy pType (pSpec ','))
               <|< ((:[]) <$> pType)

pContext :: P [EConstraint]
pContext = (pCtx <* pDRArrow) <|< pure []
  where
    pCtx = pParens (emany pType) <|< ((:[]) <$> pTypeApp)

pDRArrow :: P ()
pDRArrow = pSymbol "=>" <|< pSymbol "\x21d2"

pSRArrow :: P ()
pSRArrow = pSymbol "->" <|< pSymbol "\x2192"

pSLArrow :: P ()
pSLArrow = pSymbol "<-" <|< pSymbol "\x2190"

pConstr :: P Constr
pConstr = (Constr <$> pForall <*> pContext <*> pUIdentSym <*> pFields)
      <|< ((\ vs ct t1 c t2 -> Constr vs ct c (Left [t1, t2])) <$>
            pForall <*> pContext <*> pSAType <*> pUSymOper <*> pSAType)

pFields :: P (Either [SType] [(Ident, SType)])
pFields = Left  <$> emany pSAType
      <|> Right <$> (pSpec '{' *> (concatMap flat <$> esepBy ((,) <$> (esepBy1 pLIdentSym (pSpec ',') <* pSymbol "::") <*> pSType) (pSpec ',') <* pSpec '}'))
  where flat (is, t) = map (,t) is

pSAType :: P (Bool, EType)
pSAType = (,) <$> pStrict <*> pAType
pSType :: P (Bool, EType)
pSType  = (,) <$> pStrict <*> pType
pStrict :: P Bool
pStrict = (True <$ pSpec '!') <|< pure False

pLHS :: P LHS
pLHS = (,) <$> pTypeIdentSym <*> emany pIdKind
    <|< (\ a c b -> (c, [a,b])) <$> pIdKind <*> pSymOper <*> pIdKind

pImportSpec :: P ImportSpec
pImportSpec =
  let
    pQua = (True <$ pKeyword "qualified") <|< pure False
  in  ImportSpec <$> pQua <*> pUQIdentA <*> eoptional (pKeyword "as" *> pUQIdent) <*>
        eoptional ((,) <$> ((True <$ pKeyword "hiding") <|> pure False) <*> pParens (esepEndBy pImportItem (pSpec ',')))

pImportItem :: P ImportItem
pImportItem =
      ImpTypeCon <$> (pUQIdentSym <* pSpec '(' <* pConList <* pSpec ')')
  <|< ImpType <$> pUQIdentSym
  <|< ImpValue <$> pLQIdentSym

pConList :: P ()
pConList = pSymbol ".." <|< (() <$ esepBy1 pQIdent (pSpec ','))   -- XXX treat list as ..

--------
-- Types

pIdKind :: P IdKind
pIdKind =
      ((\ i -> IdKind i (EVar dummyIdent)) <$> pLIdentSym)          -- dummyIdent indicates that we have no kind info
  <|< pParens (IdKind <$> pLIdentSym <*> (pSymbol "::" *> pKind))

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
  pure $ if null vs then t else EForall vs t

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
  mt <- eoptional (pSymbol "::" *> pType)
  let
    r = foldl EApp f as
  pure $ maybe r (ESign r) mt

pAType :: P Expr
pAType =
      (EVar <$> pLQIdentSym)
  <|< (EVar <$> pUQIdentSym)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy1 pType (pSpec ',') <* pSpec ')'))
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
         (EAt i <$> (pSymbol "@" *> pAPat)) <|< pure (EVar i)
      )
  <|< (evar <$> pUQIdentSym <*> optional pUpdate)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy1 pPat (pSpec ',') <* pSpec ')'))
  <|< (EListish . LList <$> (pSpec '[' *> esepBy1 pPat (pSpec ',') <* pSpec ']'))
  <|< (EViewPat <$> (pSpec '(' *> pAExpr) <*> (pSRArrow *> pAPat <* pSpec ')'))
  <|< (ELazy True  <$> (pSpec '~' *> pAPat))
  <|< (ELazy False <$> (pSpec '!' *> pAPat))
  where evar v Nothing = EVar v
        evar v (Just upd) = EUpdate (EVar v) upd

pPat :: P EPat
pPat = pPatOp

pPatOp :: P EPat
pPatOp = pOperators pUOper pPatArg

pPatArg :: P EPat
pPatArg = (pSymbol "-" *> (ENegApp <$> pNumLit)) <|< pPatApp

pPatApp :: P EPat
pPatApp = do
  f <- pAPat
  as <- emany pAPat
  guard (null as || isPConApp f)
  mt <- eoptional (pSymbol "::" *> pType)
  let
    r = foldl EApp f as
  pure $ maybe r (ESign r) mt

pPatNotVar :: P EPat
pPatNotVar = do
  p <- pPat
  guard (isPConApp p)
  pure p

-------------

pEqns :: P (Ident, [Eqn])
pEqns = do
  (name, eqn@(Eqn ps alts)) <- pEqn (\ _ _ -> True)
  case (ps, alts) of
    ([], EAlts [_] []) ->
      -- don't collect equations when of the form 'i = e'
      pure (name, [eqn])
    _ -> do
      neqns <- emany (pSpec ';' *> pEqn (\ n l -> n == name && l == length ps))
      pure (name, eqn : map snd neqns)

pEqn :: (Ident -> Int -> Bool) -> P (Ident, Eqn)
pEqn test = do
  (name, pats) <- pEqnLHS
  alts <- pAlts (pSymbol "=")
  guard (test name (length pats))
  pure (name, Eqn pats alts)

pEqnLHS :: P (Ident, [EPat])
pEqnLHS =
  ((,) <$> pLIdentSym <*> emany pAPat)
  <|>   -- XXX this <|> causes a slowdown, but is necessary
  pOpLHS
  <|<
  ((\ (i, ps1) ps2 -> (i, ps1 ++ ps2)) <$> pParens pOpLHS <*> emany pAPat)
  where
    pOpLHS = (\ p1 i p2 -> (i, [p1,p2])) <$> pPatApp <*> pLOper <*> pPatApp
    pLOper = do
      i <- pOper
      guard (not (isConIdent i))
      pure i

pAlts :: P () -> P EAlts
pAlts sep = do
  alts <- pAltsL sep
  bs <- pWhere pBind
  pure (EAlts alts bs)
  
pAltsL :: P () -> P [EAlt]
pAltsL sep =
      esome ((,) <$> (pSymbol "|" *> esepBy1 pStmt (pSpec ',')) <*> (sep *> pExpr))
  <|< ((\ e -> [([], e)]) <$> (sep *> pExpr))

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
  mt <- eoptional (pSymbol "::" *> pType)
  let
    r = foldl EApp f as
  pure $ maybe r (ESign r) mt

pLam :: P Expr
pLam = eLam <$> (pSymbol "\\" *> esome pAPat) <*> (pSRArrow *> pExpr)

pCase :: P Expr
pCase = ECase <$> (pKeyword "case" *> pExpr) <*> (pKeyword "of" *> pBlock pCaseArm)

pCaseArm :: P ECaseArm
pCaseArm = (,) <$> pPat <*> pAlts pSRArrow

pLet :: P Expr
pLet = ELet <$> (pKeyword "let" *> pBlock pBind) <*> (pKeyword "in" *> pExpr)

pDo :: P Expr
pDo = EDo <$> ((Just <$> pQualDo) <|< (Nothing <$ pKeyword "do")) <*> pBlock pStmt

pIf :: P Expr
pIf = EIf <$> (pKeyword "if" *> pExpr) <*> (pKeyword "then" *> pExpr) <*> (pKeyword "else" *> pExpr)

pQualDo :: P Ident
pQualDo = do
  fn <- getFileName
  let
    is (TIdent loc qs@(_:_) "do") = Just (mkIdentLoc fn loc (intercalate "." qs))
    is _ = Nothing
  satisfyM "QualDo" is

pOperComma :: P Ident
pOperComma = pOper <|< pComma
  where
    pComma = mkIdentLoc <$> getFileName <*> getLoc <*> ("," <$ pSpec ',')

-- No right section for '-'.
pOperCommaNoMinus :: P Ident
pOperCommaNoMinus = do
  i <- pOperComma
  guard (i /= mkIdent "-")
  pure i

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
      EField fs <$> (pSymbol "=" *> pExpr) <|< pure (EFieldPun fs)
     <|<
      (EFieldWild <$ pSymbol "..")

pSelect :: P Ident
pSelect = pSpec '.' *> pLIdent

pAExpr' :: P Expr
pAExpr' = (
      (EVar   <$> pLQIdentSym)
  <|< (EVar   <$> pUQIdentSym)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy1 pExpr (pSpec ',') <* pSpec ')'))
  <|< EListish <$> (pSpec '[' *> pListish <* pSpec ']')
  <|< (ESectL <$> (pSpec '(' *> pExprArg) <*> (pOperComma <* pSpec ')'))
  <|< (ESectR <$> (pSpec '(' *> pOperCommaNoMinus) <*> (pExprArg <* pSpec ')'))
  <|< (ESelect <$> (pSpec '(' *> esome pSelect <* pSpec ')'))
  <|< (ELit noSLoc . LPrim <$> (pKeyword "primitive" *> pString))
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
   <|< (LCompr e1 <$> (pSymbol "|" *> esepBy1 pStmt (pSpec ',')))
   <|< (LFromTo e1 <$> (pSymbol ".." *> pExpr))
   <|< (LFrom e1 <$ pSymbol "..")
   <|< pure (LList [e1])

pExprOp :: P Expr
pExprOp = pOperators pOper pExprArgNeg

pExprArgNeg :: P Expr
pExprArgNeg = (pSymbol "-" *> (ENegApp <$> pExprArg)) <|< pExprArg

pOperators :: P Ident -> P Expr -> P Expr
pOperators oper one = eOper <$> one <*> emany ((,) <$> oper <*> one)
  where eOper e [] | notNeg e = e
        eOper e ies = EOper e ies
        notNeg (ENegApp _) = False
        notNeg _ = True

-------------
-- Bindings

pBind :: P EBind
pBind = 
      BPat         <$> (pPatNotVar <* pSymbol "=") <*> pExpr
  <|< pClsBind

pClsBind :: P EBind
pClsBind = 
      uncurry BFcn <$> pEqns
  <|< BSign        <$> (pLIdentSym <* pSymbol "::") <*> pType

-------------

eTuple :: [Expr] -> Expr
eTuple [] = error "eTuple"
eTuple [e] = e
eTuple es = ETuple es

isAlpha_ :: Char -> Bool
isAlpha_ c = isLower_ c || isUpper c

qualName :: FilePath -> Loc -> [String] -> String -> Ident
qualName fn loc qs s = mkIdentLoc fn loc (intercalate "." (qs ++ [s]))

-------------

formatFailed :: String -> [Token] -> LastFail Token -> String
formatFailed fn _fs (LastFail _ ts msgs) =
  let
    (line, col) = tokensLoc ts
    sloc = SLoc fn line col
  in
    showSLoc sloc ++ ":\n"
      ++ "  found:    " ++ head (map showToken ts ++ ["EOF"]) ++ "\n"
      ++ "  expected: " ++ unwords (nub msgs)
