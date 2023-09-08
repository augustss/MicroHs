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
import MicroHs.Ident
--Ximport Compat


type P a = Prsr FilePath Token a

getFileName :: P FilePath
getFileName = get

parseDie :: forall a . --X (Show a) =>
            P a -> FilePath -> String -> a
parseDie p fn file =
  let { ts = lexTop file } in
--  trace (show ts) $
  case runPrsr fn p ts of
    Left lf -> error $ formatFailed fn ts lf
    Right [(a, _)] -> a
    Right as -> error $ "Ambiguous:"
--X                     ++ unlines (map (show . fst) as)

getLoc :: P Loc
getLoc = P.do
  t <- nextToken
  P.pure (tokensLoc [t])

pTop :: P EModule
pTop = pModule <* eof

pModule :: P EModule
pModule = EModule <$> (pKeyword "module" *> pUQIdentA) <*>
                      (pSpec '(' *> esepBy pExportSpec (pSpec ',') <* pSpec ')') <*>
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
  <|> pUIdentSpecial

pUIdentSym :: P Ident
pUIdentSym = pUIdent <|< pParens pUSymOper

pUIdentSpecial :: P Ident
pUIdentSpecial = P.do
  fn <- getFileName
  loc <- getLoc
  let
    mk = mkIdentLoc fn loc
  
  (mk . map (const ',') <$> (pSpec '(' *> some (pSpec ',') <* pSpec ')'))
    <|> (mk "()" <$ (pSpec '(' *> pSpec ')'))  -- Allow () as a constructor name
    <|> (mk "[]" <$ (pSpec '[' *> pSpec ']'))  -- Allow [] as a constructor name

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
  <|> pUIdentSpecial

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

pLSymOper :: P Ident
pLSymOper = P.do
  s <- pSymOper
  guard (not (isUOper s))
  P.pure s

reservedOps :: [String]
reservedOps = ["=", "|", "::", "<-", "@", ".."]

pUQIdentSym :: P Ident
pUQIdentSym = pUQIdent <|< pParens pUQSymOper

pLQIdentSym :: P Ident
pLQIdentSym = pLQIdent <|< pParens pLQSymOper 

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
    is _ = Nothing
  satisfyM "literal" is

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
pLHS = pair <$> pUIdentSym <*> many pIdKind

pImportSpec :: P ImportSpec
pImportSpec =
  let
    pQua = (True <$ pKeyword "qualified") <|< pure False
  in  ImportSpec <$> pQua <*> pUQIdentA <*> optional (pKeyword "as" *> pUQIdent)

--------
-- Types

pIdKind :: P IdKind
pIdKind =
      ((\ i -> IdKind i kType) <$> pLIdentSym)
  <|> pParens (IdKind <$> pLIdentSym <*> (pSymbol "::" *> pKind))

pKind :: P EKind
pKind = pType

pTypeScheme :: P ETypeScheme
pTypeScheme = P.do
  vs <- (pKeyword "forall" *> esome pIdKind <* pSymbol ".") <|< pure []
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
  mt <- optional (pSymbol "::" *> pType)
  let
    r = foldl EApp f as
  pure $ maybe r (ESign r) mt

pAType :: P Expr
pAType =
      (EVar <$> pLQIdentSym)
  <|> (EVar <$> pUQIdentSym)
  <|> pLit
  <|> (eTuple <$> (pSpec '(' *> esepBy1 pType (pSpec ',') <* pSpec ')'))
  <|> (EListish . LList . (:[]) <$> (pSpec '[' *> pType <* pSpec ']'))  -- Unlike expressions, only allow a single element.

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
  <|> pLit
  <|> (eTuple <$> (pSpec '(' *> esepBy1 pPat (pSpec ',') <* pSpec ')'))
  <|> (EListish . LList <$> (pSpec '[' *> esepBy1 pPat (pSpec ',') <* pSpec ']'))
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
  mt <- optional (pSymbol "::" *> pType)
  let
    r = foldl EApp f as
  pure $ maybe r (ESign r) mt

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

pQualDo :: P Ident
pQualDo = P.do
  fn <- getFileName
  let
    is (TIdent loc qs@(_:_) "do") = Just (mkIdentLoc fn loc (intercalate "." qs))
    is _ = Nothing
  satisfyM "QualDo" is

pAExpr :: P Expr
pAExpr = (
      (EVar   <$> pLQIdentSym)
  <|> (EVar   <$> pUQIdentSym)
  <|> pLit
  <|> (eTuple <$> (pSpec '(' *> esepBy1 pExpr (pSpec ',') <* pSpec ')'))
  <|> EListish <$> (pSpec '[' *> pListish <* pSpec ']')
  <|> (ESectL <$> (pSpec '(' *> pExprArg) <*> (pOper <* pSpec ')'))
  <|> (ESectR <$> (pSpec '(' *> pOper) <*> (pExprArg <* pSpec ')'))
  <|> (ELit noSLoc . LPrim <$> (pKeyword "primitive" *> pString))
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
pExprOp =
  let
    p10 = pExprArg
    p9 = pRightAssoc (pOpers ["."]) $
         pLeftAssoc  (pOpers ["?", "!!", "<?>"]) p10
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

pRightAssoc :: P Ident -> P Expr -> P Expr
pRightAssoc pOp p = P.do
  e1 <- p
  let
    rest =
      P.do
        op <- pOp
        e2 <- pRightAssoc pOp p
        pure $ appOp op e1 e2
  rest <|< pure e1

pNonAssoc :: P Ident -> P Expr -> P Expr
pNonAssoc pOp p = P.do
  e1 <- p
  let
    rest =
      P.do
        op <- pOp
        e2 <- p
        pure $ appOp op e1 e2
  rest <|< pure e1

pLeftAssoc :: P Ident -> P Expr -> P Expr
pLeftAssoc pOp p = P.do
  e1 <- p
  es <- emany (pair <$> pOp <*> p)
  pure $ foldl (\ x (op, y) -> appOp op x y) e1 es

pOpers :: [String] -> P Ident
pOpers ops = P.do
  op <- pOper
  guard (elemBy eqString (unIdent op) ops)
  pure op

-------------

eTuple :: [Expr] -> Expr
eTuple [] = undefined
eTuple [e] = e
eTuple es = ETuple es

appOp :: Ident -> Expr -> Expr -> Expr
appOp op e1 e2 = EApp (EApp (EVar op) e1) e2

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
