-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-do-bind #-}
module MicroHs.Parse(pTop, parseDie) where
import Prelude --Xhiding (Monad(..), Applicative(..), MonadFail(..), Functor(..), (<$>), showString, showChar, showList)
--import Control.Monad
--import Control.Monad.State.Strict
--import Control.Applicative --hiding (many, some)
import Data.Char
import Data.List
import Text.ParserComb as P
--import Debug.Trace
--import MicroHs.Lex
import MicroHs.Lex
import MicroHs.Expr
--Ximport Compat


type P a = Prsr () Token a

parseDie :: forall a . --X (Show a) =>
            P a -> String -> String -> a
parseDie p fn file =
  let { ts = lexTop file } in
--  trace (show ts) $
  case runPrsr () p ts of
    Left lf -> error $ formatFailed fn ts lf
    Right [(a, _)] -> a
    Right as -> error $ "Ambiguous:"
--X                     ++ unlines (map (show . fst) as)

pTop :: P EModule
pTop = pModule <* eof

pModule :: P EModule
pModule = EModule <$> (pKeyword "module" *> pUQIdentA) <*>
                      (pSpec '(' *> esepBy pExportSpec (pSpec ',') <* pSpec ')') <*>
                      (pKeyword "where" *> pBlock pDef)

pQIdent :: P Ident
pQIdent = satisfyM "QIdent" is
  where
    is (TIdent _ qs s) | isAlpha_ (head s) = Just (qualName qs s)
    is _ = Nothing

pUIdentA :: P Ident
pUIdentA = satisfyM "UIdent" is
  where
    is (TIdent _ [] s) | isUpper (head s) = Just s
    is _ = Nothing

pUIdent :: P Ident
pUIdent =
      pUIdentA
  <|> pUIdentSpecial

pUIdentSym :: P Ident
pUIdentSym = pUIdent <|< pParens pUSymOper

pUIdentSpecial :: P Ident
pUIdentSpecial =
      (map (const ',') <$> (pSpec '(' *> some (pSpec ',') <* pSpec ')'))
  <|> ("()" <$ (pSpec '(' *> pSpec ')'))  -- Allow () as a constructor name
  <|> ("[]" <$ (pSpec '[' *> pSpec ']'))  -- Allow [] as a constructor name

pUQIdentA :: P Ident
pUQIdentA = satisfyM "UQIdent" is
  where
    is (TIdent _ qs s) | isUpper (head s) = Just (qualName qs s)
    is _ = Nothing

pUQIdent :: P Ident
pUQIdent =
      pUQIdentA
  <|> pUIdentSpecial

pLIdent :: P Ident
pLIdent = satisfyM "LIdent" is
  where
    is (TIdent _ [] s) | isLower_ (head s) && not (elemBy eqString s keywords) = Just s
    is _ = Nothing

pLQIdent :: P Ident
pLQIdent = satisfyM "LQIdent" is
  where
    is (TIdent _ qs s) | isLower_ (head s) && not (elemBy eqString s keywords) = Just (qualName qs s)
    is _ = Nothing

keywords :: [String]
keywords = ["case", "data", "do", "else", "forall", "if", "import",
  "in", "let", "module", "newtype", "of", "primitive", "then", "type", "where"]

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

pOper :: P String
pOper = pQSymOper <|< (pSpec '`' *> pQIdent <* pSpec '`')

pQSymOper :: P Ident
pQSymOper = satisfyM "QSymOper" is
  where
    is (TIdent _ qs s) | not (isAlpha_ (head s)) && not (elemBy eqString s reservedOps) = Just (qualName qs s)
    is _ = Nothing

pSymOper :: P Ident
pSymOper = satisfyM "SymOper" is
  where
    is (TIdent _ [] s) | not (isAlpha_ (head s)) && not (elemBy eqString s reservedOps) = Just s
    is _ = Nothing

pUQSymOper :: P Ident
pUQSymOper = P.do
  s <- pQSymOper
  guard (eqChar (head s) ':')
  P.pure s

pUSymOper :: P Ident
pUSymOper = P.do
  s <- pSymOper
  guard (eqChar (head s) ':')
  P.pure s

pLQSymOper :: P Ident
pLQSymOper = P.do
  s <- pQSymOper
  guard (neChar (head s) ':')
  P.pure s

pLSymOper :: P Ident
pLSymOper = P.do
  s <- pSymOper
  guard (neChar (head s) ':')
  P.pure s

reservedOps :: [String]
reservedOps = ["=", "|", "::", "<-", "@"]

pUQIdentSym :: P Ident
pUQIdentSym = pUQIdent <|< pParens pUQSymOper

pLQIdentSym :: P Ident
pLQIdentSym = pLQIdent <|< pParens pLQSymOper 

pLIdentSym :: P Ident
pLIdentSym = pLIdent <|< pParens pLSymOper

pParens :: forall a . P a -> P a
pParens p = pSpec '(' *> p <* pSpec ')'

pLit :: P Lit
pLit = satisfyM "Lit" is
  where
    is (TString _ s) = Just (LStr s)
    is (TChar _ c) = Just (LChar c)
    is (TInt _ i) = Just (LInt i)
    is _ = Nothing

pString :: P String
pString = satisfyM "string" is
  where
    is (TString _ s) = Just s
    is _ = Nothing

---------------

pExportSpec :: P ExportSpec
pExportSpec =
      ExpModule <$> (pKeyword "module" *> pUQIdent)
  <|> ExpTypeCon <$> (pUQIdentSym <* pSpec '(' <* pSymbol ".." <* pSpec ')')
  <|> ExpType <$> pUQIdentSym
  <|> ExpValue <$> pLQIdentSym

pKeyword :: String -> P ()
pKeyword kw = () <$ satisfy kw is
  where
    is (TIdent _ [] s) = eqString kw s
    is _ = False

pBlock :: forall a . P a -> P [a]
pBlock p = P.do
  pSpec '{'
  as <- esepBy p (pSpec ';')
  optional (pSpec ';')
  pSpec '}'
  pure as

pDef :: P EDef
pDef =
      Data        <$> (pKeyword "data"    *> pLHS <* pSymbol "=") <*> esepBy1 (pair <$> pUIdentSym <*> many pAType) (pSymbol "|")
  <|> Newtype     <$> (pKeyword "newtype" *> pLHS <* pSymbol "=") <*> pUIdent <*> pAType
  <|> Type        <$> (pKeyword "type"    *> pLHS <* pSymbol "=") <*> pType
  <|> uncurry Fcn <$> pEqns
  <|> Sign        <$> (pLIdentSym <* pSymbol "::") <*> pTypeScheme
  <|> Import      <$> (pKeyword "import" *> pImportSpec)

pLHS :: P LHS
pLHS = pair <$> pUIdentSym <*> many pLIdentSym

pImportSpec :: P ImportSpec
pImportSpec =
  let
    pQua = (True <$ pKeyword "qualified") <|< pure False
  in  ImportSpec <$> pQua <*> pUQIdentA <*> optional (pKeyword "as" *> pUQIdent)

--------
-- Types

pTypeScheme :: P ETypeScheme
pTypeScheme = P.do
  vs <- (pKeyword "forall" *> esome pLIdentSym <* pSymbol ".") <|< pure []
  t <- pType
  pure $ ETypeScheme vs t

--
-- Partial copy of pExpr, but that includes '->'.
-- Including '->' in pExprOp interacts poorly with '->'
-- in lambda and 'case'.
pType :: P EType
pType = pTypeOp

pTypeOp :: P EType
pTypeOp =
  let
{-
    p10 = pTypeArg
    p9 = p10
    p8 = p9
    p7 = p8
    p6 = p7
    p5 = p6
    p4 = p5
    p3 = p4
    p2 = p3
    p1 = p2
    p0 = pRightAssoc (pOpers ["->"]) p1
-}
    p0 = pRightAssoc (pOpers ["->"]) pTypeArg
  in  p0

pTypeArg :: P EType
pTypeArg = pTypeApp

pTypeApp :: P EType
pTypeApp = P.do
  f <- pAType
  as <- emany pAType
  pure $ foldl EApp f as

pAType :: P Expr
pAType =
      (EVar <$> pLQIdentSym)
  <|> (EVar <$> pUQIdentSym)
  <|> (ELit <$> pLit)
  <|> (eTuple <$> (pSpec '(' *> esepBy1 pType (pSpec ',') <* pSpec ')'))
  <|> (EList . (:[]) <$> (pSpec '[' *> pType <* pSpec ']'))  -- Unlike expressions, only allow a single element.

-------------
-- Patterns

-- Sadly pattern and expression parsing cannot be joined because the
-- use of '->' in 'case' and lambda makes it weird.
-- Instead this is just a copy of some of the expression rules.
-- XXX This can probably be joined with pExpr again now that pType
-- is separate.
pAPat :: P EPat
pAPat =
      (EVar <$> pLIdentSym)
  <|> (EVar <$> pUQIdentSym)
  <|> (ELit <$> pLit)
  <|> (eTuple <$> (pSpec '(' *> esepBy1 pPat (pSpec ',') <* pSpec ')'))
  <|> (EList <$> (pSpec '[' *> esepBy1 pPat (pSpec ',') <* pSpec ']'))
  <|> (EAt <$> (pLIdentSym <* pSymbol "@") <*> pAPat)

pPat :: P EPat
pPat = pPatOp

pPatOp :: P EPat
pPatOp =
  let
{-
    p10 = pPatArg
    p9 = p10
    p8 = p9
    p7 = p8
    p6 = p7
    p5 = pRightAssoc (pOpers [":"]) p6
    p4 = p5
    p3 = p4
    p2 = p3
    p1 = p2
    p0 = p1
-}
    p0 = pRightAssoc (pOpers [":"]) pPatArg
  in  p0

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
      esome (pair <$> (pSymbol "|" *> esepBy1 pStmt (pSpec ',')) <*> (sep *> pExpr))
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
  <|> (SLet  <$> (pKeyword "let" *> pBlock pBind))
  <|> (SThen <$> pExpr)

-------------
-- Expressions

pExpr :: P Expr
pExpr = pExprOp

pExprArg :: P Expr
pExprArg = pExprApp <|> pLam <|> pCase <|> pLet <|> pIf <|> pDo

pExprApp :: P Expr
pExprApp = P.do
  f <- pAExpr
  as <- emany pAExpr
  pure $ foldl EApp f as

pLam :: P Expr
pLam = ELam <$> (pSymbol "\\" *> esome pAPat) <*> (pSymbol "->" *> pExpr)

pCase :: P Expr
pCase = ECase <$> (pKeyword "case" *> pExpr) <*> (pKeyword "of" *> pBlock pCaseArm)

pCaseArm :: P ECaseArm
pCaseArm = pair <$> pPat <*> pAlts (pSymbol "->")

pLet :: P Expr
pLet = ELet <$> (pKeyword "let" *> pBlock pBind) <*> (pKeyword "in" *> pExpr)

pDo :: P Expr
pDo = EDo <$> ((Just <$> pQualDo) <|< (Nothing <$ pKeyword "do")) <*> pBlock pStmt

pIf :: P Expr
pIf = EIf <$> (pKeyword "if" *> pExpr) <*> (pKeyword "then" *> pExpr) <*> (pKeyword "else" *> pExpr)

pQualDo :: P String
pQualDo = satisfyM "QualDo" is
  where
    is (TIdent _ qs@(_:_) "do") = Just (intercalate "." qs)
    is _ = Nothing

pAExpr :: P Expr
pAExpr =
      (EVar   <$> pLQIdentSym)
  <|> (EVar   <$> pUQIdentSym)
  <|> (ELit   <$> pLit)
  <|> (eTuple <$> (pSpec '(' *> esepBy1 pExpr (pSpec ',') <* pSpec ')'))
  <|> (EList  <$> (pSpec '[' *> esepBy1 pExpr (pSpec ',') <* pSpec ']'))
  <|> (ESectL <$> (pSpec '(' *> pExprArg) <*> (pOper <* pSpec ')'))
  <|> (ESectR <$> (pSpec '(' *> pOper) <*> (pExprArg <* pSpec ')'))
  <|> (ECompr <$> (pSpec '[' *> pExpr <* pSymbol "|") <*> (esepBy1 pStmt (pSpec ',') <* pSpec ']'))
  <|> (ELit . LPrim <$> (pKeyword "primitive" *> pString))

pExprOp :: P Expr
pExprOp =
  let
    p10 = pExprArg
    p9 = pRightAssoc (pOpers ["."]) $
         pLeftAssoc  (pOpers ["?", "!!"]) p10
    p8 = p9
    p7 = pLeftAssoc  (pOpers ["*", "quot", "rem"]) p8
    p6 = pLeftAssoc  (pOpers ["+", "-"]) p7
    p5 = pRightAssoc (pOpers [":", "++"]) p6
    p4 = pNonAssoc   (pOpers ["==", "/=", "<", "<=", ">", ">="]) $
         pLeftAssoc  (pOpers ["<*>", "<*", "*>", "<$>", "<$"])   p5
    p3 = pRightAssoc (pOpers ["&&"]) $
         pLeftAssoc  (pOpers ["<|>","<|<"]) p4
    p2 = pRightAssoc (pOpers ["||"]) p3
    p1 = pLeftAssoc  (pOpers [">>=", ">>"]) p2
    p0 = pRightAssoc (pOpers ["$"]) p1
  in  p0

-------------
-- Bindings

pBind :: P EBind
pBind = 
      uncurry BFcn <$> pEqns
  <|> BPat <$> (pPatNotVar <* pSymbol "=") <*> pExpr

-------------

pRightAssoc :: P String -> P Expr -> P Expr
pRightAssoc pOp p = P.do
  e1 <- p
  let
    rest =
      P.do
        op <- pOp
        e2 <- pRightAssoc pOp p
        pure $ appOp op e1 e2
  rest <|< pure e1

pNonAssoc :: P String -> P Expr -> P Expr
pNonAssoc pOp p = P.do
  e1 <- p
  let
    rest =
      P.do
        op <- pOp
        e2 <- p
        pure $ appOp op e1 e2
  rest <|< pure e1

pLeftAssoc :: P String -> P Expr -> P Expr
pLeftAssoc pOp p = P.do
  e1 <- p
  es <- emany (pair <$> pOp <*> p)
  pure $ foldl (\ x (op, y) -> appOp op x y) e1 es

pOpers :: [String] -> P String
pOpers ops = P.do
  op <- pOper
  guard (elemBy eqString op ops)
  pure op

-------------

eTuple :: [Expr] -> Expr
eTuple [] = undefined
eTuple [e] = e
eTuple es = ETuple es

appOp :: String -> Expr -> Expr -> Expr
appOp op e1 e2 = EApp (EApp (EVar op) e1) e2

isAlpha_ :: Char -> Bool
isAlpha_ c = isLower_ c || isUpper c

qualName :: [String] -> String -> String
qualName qs s = intercalate "." (qs ++ [s])

-------------

formatFailed :: String -> [Token] -> LastFail Token -> String
formatFailed fn _fs _lf@(LastFail _ ts _msgs) =
  let
    loc = tokensLoc ts
    line = getLin loc
    col = getCol loc
  in
    showString fn ++ ": "
         ++ "line " ++ showInt line ++ ", col " ++ showInt col ++ ":\n"
--         ++ "   found: " ++ tokenString (head ts)
--         ++ show lf ++ "\n"
--         ++ show fs

--tokenString :: Token -> String
--tokenString 
