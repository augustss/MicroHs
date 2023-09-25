-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-do-bind #-}
module MicroHs.Parse(pTop, parseDie, parse, pExprTop) where
import Prelude --Xhiding (Monad(..), Applicative(..), MonadFail(..), Functor(..), (<$>), showString, showChar, showList)
import Data.Char
import Data.List
import Text.ParserComb as P
import MicroHs.Lex
import MicroHs.Expr
import MicroHs.Ident
--Ximport Compat


type P a = Prsr FilePath Token a

getFileName :: P FilePath
getFileName = get

parseDie :: forall a . --X (Show a) =>
            P a -> FilePath -> String -> a
parseDie p fn file =
  case parse p fn file of
    Left msg -> error msg
    Right a -> a

parse :: forall a . --X (Show a) =>
         P a -> FilePath -> String -> Either String a
parse p fn file =
  let { ts = lexTop file } in
  case runPrsr fn p ts of
    Left lf -> Left $ formatFailed fn ts lf
    Right [(a, _)] -> Right a
    Right as -> Left $ "Ambiguous:"
--X                     ++ unlines (map (show . fst) as)

getLoc :: P Loc
getLoc = P.do
  t <- nextToken
  P.pure (tokensLoc [t])

pTop :: P EModule
pTop = pModule <* eof

pExprTop :: P Expr
pExprTop = pExpr <* eof

pModule :: P EModule
pModule = EModule <$> (pKeyword "module" *> pUQIdentA) <*>
                      (pSpec '(' *> esepEndBy pExportItem (pSpec ',') <* pSpec ')') <*>
                      (pKeyword "where" *> pBlock pDef)

pQIdent :: P Ident
pQIdent = P.do
  fn <- getFileName
  let
    is (TIdent loc qs s) | isAlpha_ (head s) = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "QIdent" is

pUIdentA :: P Ident
pUIdentA = P.do
  fn <- getFileName
  let
    is (TIdent loc [] s) | isUpper (head s) = Just (mkIdentLoc fn loc s)
    is _ = Nothing
  satisfyM "UIdent" is

pUIdent :: P Ident
pUIdent =
      pUIdentA
  <|< pUIdentSpecial

pUIdentSym :: P Ident
pUIdentSym = pUIdent <|< pParens pUSymOper

pUIdentSpecial :: P Ident
pUIdentSpecial = P.do
  fn <- getFileName
  loc <- getLoc
  let
    mk = mkIdentLoc fn loc
  
  (mk . map (const ',') <$> (pSpec '(' *> esome (pSpec ',') <* pSpec ')'))
    <|< (mk "()" <$ (pSpec '(' *> pSpec ')'))  -- Allow () as a constructor name
    <|< (mk "[]" <$ (pSpec '[' *> pSpec ']'))  -- Allow [] as a constructor name

pUQIdentA :: P Ident
pUQIdentA = P.do
  fn <- getFileName
  let
    is (TIdent loc qs s) | isUpper (head s) = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "UQIdent" is

pUQIdent :: P Ident
pUQIdent =
      pUQIdentA
  <|< pUIdentSpecial

pLIdent :: P Ident
pLIdent = P.do
  fn <- getFileName
  let
    is (TIdent loc [] s) | isLower_ (head s) && not (elemBy eqString s keywords) = Just (mkIdentLoc fn loc s)
    is _ = Nothing
  satisfyM "LIdent" is

pLQIdent :: P Ident
pLQIdent = P.do
  fn <- getFileName
  let
    is (TIdent loc qs s) | isLower_ (head s) && not (elemBy eqString s keywords) = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "LQIdent" is

keywords :: [String]
keywords = ["case", "data", "do", "else", "forall", "foreign", "if", "import",
  "in", "infix", "infixl", "infixr",
  "let", "module", "newtype", "of", "primitive", "then", "type", "where"]

pSpec :: Char -> P ()
pSpec c = () <$ satisfy [c] is
  where
    is (TSpec _ d) = eqChar c d
    is _ = False

pSymbol :: String -> P ()
pSymbol sym = () <$ satisfy sym is
  where
    is (TIdent _ [] s) = eqString s sym
    is _ = False

pOper :: P Ident
pOper = pQSymOper <|< (pSpec '`' *> pQIdent <* pSpec '`')

pQSymOper :: P Ident
pQSymOper = P.do
  fn <- getFileName
  let
    is (TIdent loc qs s) | not (isAlpha_ (head s)) && not (elemBy eqString s reservedOps) = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "QSymOper" is

pSymOper :: P Ident
pSymOper = P.do
  fn <- getFileName
  let
    is (TIdent loc [] s) | not (isAlpha_ (head s)) && not (elemBy eqString s reservedOps) = Just (mkIdentLoc fn loc s)
    is _ = Nothing
  satisfyM "SymOper" is

pUQSymOper :: P Ident
pUQSymOper = P.do
  s <- pQSymOper
  guard (isUOper s)
  P.pure s

isUOper :: Ident -> Bool
isUOper = eqChar ':' . head . unIdent

pUSymOper :: P Ident
pUSymOper = P.do
  s <- pSymOper
  guard (isUOper s)
  P.pure s

pLQSymOper :: P Ident
pLQSymOper = P.do
  s <- pQSymOper
  guard (not (isUOper s))
  P.pure s

-- Allow -> as well
pLQSymOperArr :: P Ident
pLQSymOperArr = pLQSymOper <|< pQArrow

-- Parse ->, possibly qualified
pQArrow :: P Ident
pQArrow = P.do
  fn <- getFileName
  let
    is (TIdent loc qs s@"->") = Just (qualName fn loc qs s)
    is _ = Nothing
  satisfyM "->" is

pLSymOper :: P Ident
pLSymOper = P.do
  s <- pSymOper
  guard (not (isUOper s))
  P.pure s

reservedOps :: [String]
reservedOps = ["=", "|", "::", "<-", "@", "..", "->"]

pUQIdentSym :: P Ident
pUQIdentSym = pUQIdent <|< pParens pUQSymOper

pLQIdentSym :: P Ident
pLQIdentSym = pLQIdent <|< pParens pLQSymOperArr

pLIdentSym :: P Ident
pLIdentSym = pLIdent <|< pParens pLSymOper

pParens :: forall a . P a -> P a
pParens p = pSpec '(' *> p <* pSpec ')'

pLit :: P Expr
pLit = P.do
  fn <- getFileName
  let
    is (TString (l, c) s) = Just (ELit (SLoc fn l c) (LStr s))
    is (TChar   (l, c) a) = Just (ELit (SLoc fn l c) (LChar a))
    is (TInt    (l, c) i) = Just (ELit (SLoc fn l c) (LInt i))
    is (TDouble (l, c) d) = Just (ELit (SLoc fn l c) (LDouble d))
    is _ = Nothing
  satisfyM "literal" is

pString :: P String
pString = satisfyM "string" is
  where
    is (TString _ s) = Just s
    is _ = Nothing

---------------

pExportItem :: P ExportItem
pExportItem =
      ExpModule <$> (pKeyword "module" *> pUQIdent)
  <|< ExpTypeCon <$> (pUQIdentSym <* pSpec '(' <* pSymbol ".." <* pSpec ')')
  <|< ExpType <$> pUQIdentSym
  <|< ExpValue <$> pLQIdentSym

pKeyword :: String -> P ()
pKeyword kw = () <$ satisfy kw is
  where
    is (TIdent _ [] s) = eqString kw s
    is _ = False

pBlock :: forall a . P a -> P [a]
pBlock p = P.do
  pSpec '{'
  as <- esepBy p (pSpec ';')
  eoptional (pSpec ';')
  pSpec '}'
  pure as

pDef :: P EDef
pDef =
      Data        <$> (pKeyword "data"    *> pLHS) <*> ((pSymbol "=" *> esepBy1 ((,) <$> pUIdentSym <*> emany pAType) (pSymbol "|"))
                                                        <|< P.pure [])
  <|< Newtype     <$> (pKeyword "newtype" *> pLHS) <*> (pSymbol "=" *> pUIdent) <*> pAType
  <|< Type        <$> (pKeyword "type"    *> pLHS) <*> (pSymbol "=" *> pType)
  <|< uncurry Fcn <$> pEqns
  <|< Sign        <$> (pLIdentSym <* pSymbol "::") <*> pTypeScheme
  <|< Import      <$> (pKeyword "import" *> pImportSpec)
  <|< ForImp      <$> (pKeyword "foreign" *> pKeyword "import" *> pKeyword "ccall" *> pString) <*> pLIdent <*> (pSymbol "::" *> pType)
  <|< Infix       <$> ((,) <$> pAssoc <*> pPrec) <*> esepBy1 pTypeOper (pSpec ',')
  where
    pAssoc = (AssocLeft <$ pKeyword "infixl") <|< (AssocRight <$ pKeyword "infixr") <|< (AssocNone <$ pKeyword "infix")
    dig (TInt _ i) | -1 <= i && i <= 9 = Just i
    dig _ = Nothing
    pPrec = satisfyM "digit" dig

pLHS :: P LHS
pLHS = (,) <$> pUIdentSym <*> emany pIdKind

pImportSpec :: P ImportSpec
pImportSpec =
  let
    pQua = (True <$ pKeyword "qualified") <|< pure False
  in  ImportSpec <$> pQua <*> pUQIdentA <*> eoptional (pKeyword "as" *> pUQIdent) <*>
        eoptional ((,) <$> ((True <$ pKeyword "hiding") <|> pure False) <*> pParens (esepEndBy pImportItem (pSpec ',')))

pImportItem :: P ImportItem
pImportItem =
      ImpTypeCon <$> (pUQIdentSym <* pSpec '(' <* pSymbol ".." <* pSpec ')')
  <|< ImpType <$> pUQIdentSym
  <|< ImpValue <$> pLQIdentSym

--------
-- Types

pIdKind :: P IdKind
pIdKind =
      ((\ i -> IdKind i kType) <$> pLIdentSym)
  <|< pParens (IdKind <$> pLIdentSym <*> (pSymbol "::" *> pKind))

pKind :: P EKind
pKind = pType

{-
pTypeScheme :: P ETypeScheme
pTypeScheme = P.do
  vs <- (pKeyword "forall" *> esome pIdKind <* pSymbol ".") <|< pure []
  t <- pType
  pure $ if null vs then t else EForall vs t
-}
pTypeScheme :: P ETypeScheme
pTypeScheme = pType

--
-- Partial copy of pExpr, but that includes '->'.
-- Including '->' in pExprOp interacts poorly with '->'
-- in lambda and 'case'.
pType :: P EType
pType = P.do
  vs <- (pKeyword "forall" *> esome pIdKind <* pSymbol ".") <|< pure []
  t <- pTypeOp
  pure $ if null vs then t else EForall vs t

pTypeOp :: P EType
pTypeOp = pOperators pTypeOper pTypeArg

pTypeOper :: P Ident
pTypeOper = pOper <|< (mkIdent "->" <$ pSymbol "->")

pTypeArg :: P EType
pTypeArg = pTypeApp

pTypeApp :: P EType
pTypeApp = P.do
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
      (P.do
         i <- pLIdentSym
         (EAt i <$> (pSymbol "@" *> pAPat)) <|< pure (EVar i)
      )
  <|< (EVar <$> pUQIdentSym)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy1 pPat (pSpec ',') <* pSpec ')'))
  <|< (EListish . LList <$> (pSpec '[' *> esepBy1 pPat (pSpec ',') <* pSpec ']'))

pPat :: P EPat
pPat = pPatOp

pPatOp :: P EPat
pPatOp = pOperators pOper pPatArg

pPatArg :: P EPat
pPatArg = pPatApp

pPatApp :: P EPat
pPatApp = P.do
  f <- pAPat
  as <- emany pAPat
  guard (null as || isPConApp f)
  pure $ foldl EApp f as

pPatNotVar :: P EPat
pPatNotVar = P.do
  p <- pPat
  guard (not (isPVar p))
  pure p

-------------

pEqns :: P (Ident, [Eqn])
pEqns = P.do
  (name, eqn@(Eqn ps _)) <- pEqn (\ _ _ -> True)
  neqns <- emany (pSpec ';' *> pEqn (\ n l -> eqIdent n name && l == length ps))
  P.pure (name, eqn : map snd neqns)

pEqn :: (Ident -> Int -> Bool) -> P (Ident, Eqn)
pEqn test = P.do
  name <- pLIdentSym
  pats <- emany pAPat
  alts <- pAlts (pSymbol "=")
  guard (test name (length pats))
  P.pure (name, Eqn pats alts)

pAlts :: P () -> P EAlts
pAlts sep = P.do
  alts <- pAltsL sep
  bs <- pWhere
  P.pure (EAlts alts bs)
  
pAltsL :: P () -> P [EAlt]
pAltsL sep =
      esome ((,) <$> (pSymbol "|" *> esepBy1 pStmt (pSpec ',')) <*> (sep *> pExpr))
  <|< ((\ e -> [([], e)]) <$> (sep *> pExpr))

pWhere :: P [EBind]
pWhere =
      (pKeyword "where" *> pBlock pBind)
  <|< P.pure []

-------------
-- Statements

pStmt :: P EStmt
pStmt =
      (SBind <$> (pPat <* pSymbol "<-") <*> pExpr)
  <|< (SLet  <$> (pKeyword "let" *> pBlock pBind))
  <|< (SThen <$> pExpr)

-------------
-- Expressions

pExpr :: P Expr
pExpr = pExprOp

pExprArg :: P Expr
pExprArg = pExprApp <|< pLam <|< pCase <|< pLet <|< pIf <|< pDo

pExprApp :: P Expr
pExprApp = P.do
  f <- pAExpr
  as <- emany pAExpr
  mt <- eoptional (pSymbol "::" *> pType)
  let
    r = foldl EApp f as
  pure $ maybe r (ESign r) mt

pLam :: P Expr
pLam = ELam <$> (pSymbol "\\" *> esome pAPat) <*> (pSymbol "->" *> pExpr)

pCase :: P Expr
pCase = ECase <$> (pKeyword "case" *> pExpr) <*> (pKeyword "of" *> pBlock pCaseArm)

pCaseArm :: P ECaseArm
pCaseArm = (,) <$> pPat <*> pAlts (pSymbol "->")

pLet :: P Expr
pLet = ELet <$> (pKeyword "let" *> pBlock pBind) <*> (pKeyword "in" *> pExpr)

pDo :: P Expr
pDo = EDo <$> ((Just <$> pQualDo) <|< (Nothing <$ pKeyword "do")) <*> pBlock pStmt

pIf :: P Expr
pIf = EIf <$> (pKeyword "if" *> pExpr) <*> (pKeyword "then" *> pExpr) <*> (pKeyword "else" *> pExpr)

pQualDo :: P Ident
pQualDo = P.do
  fn <- getFileName
  let
    is (TIdent loc qs@(_:_) "do") = Just (mkIdentLoc fn loc (intercalate "." qs))
    is _ = Nothing
  satisfyM "QualDo" is

pOperComma :: P Ident
pOperComma = pOper <|< pComma
  where
    pComma = mkIdentLoc <$> getFileName <*> getLoc <*> ("," <$ pSpec ',')

pAExpr :: P Expr
pAExpr = (
      (EVar   <$> pLQIdentSym)
  <|< (EVar   <$> pUQIdentSym)
  <|< pLit
  <|< (eTuple <$> (pSpec '(' *> esepBy1 pExpr (pSpec ',') <* pSpec ')'))
  <|< EListish <$> (pSpec '[' *> pListish <* pSpec ']')
  <|< (ESectL <$> (pSpec '(' *> pExprArg) <*> (pOperComma <* pSpec ')'))
  <|< (ESectR <$> (pSpec '(' *> pOperComma) <*> (pExprArg <* pSpec ')'))
  <|< (ELit noSLoc . LPrim <$> (pKeyword "primitive" *> pString))
  )
  -- This weirdly slows down parsing
  -- <?> "aexpr"

pListish :: P Listish
pListish = P.do
  e1 <- pExpr
  let
    pMore = P.do
      e2 <- pExpr
      ((\ es -> LList (e1:e2:es)) <$> esome (pSpec ',' *> pExpr))
       <|< (LFromThenTo e1 e2 <$> (pSymbol ".." *> pExpr))
       <|< (LFromThen e1 e2 <$ pSymbol "..")
       <|< P.pure (LList [e1,e2])
  (pSpec ',' *> pMore)
   <|< (LCompr e1 <$> (pSymbol "|" *> esepBy1 pStmt (pSpec ',')))
   <|< (LFromTo e1 <$> (pSymbol ".." *> pExpr))
   <|< (LFrom e1 <$ pSymbol "..")
   <|< P.pure (LList [e1])

pExprOp :: P Expr
pExprOp = pOperators pOper pExprArg

pOperators :: P Ident -> P Expr -> P Expr
pOperators oper one = eOper <$> one <*> emany ((,) <$> oper <*> one)
  where eOper e [] = e
        eOper e ies = EOper e ies

-------------
-- Bindings

pBind :: P EBind
pBind = 
      uncurry BFcn <$> pEqns
  <|< BPat         <$> (pPatNotVar <* pSymbol "=") <*> pExpr
  <|< BSign        <$> (pLIdentSym <* pSymbol "::") <*> pTypeScheme

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
      ++ "  expected: " ++ unwords (nubBy eqString msgs)
