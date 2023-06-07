module MicroHs.Parse(
  pTop,
  parseDie,
  Ident,
  IdentModule,
  qual,
  EDef(..),
  ImportSpec(..),
  Expr(..),
  EStmt(..),
  EPat(..),
  EModule(..),
  ExportSpec(..),
  ) where
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Text.ParserComb
import Control.Applicative --hiding (many, some)
--import Debug.Trace


type P a = Prsr [Int] a

type Ident = String
type IdentModule = Ident

data EDef
  = Data LHS [Constr]
  | Fcn LHS Expr
  | Sign Ident Type
  | Import ImportSpec
  deriving (Show)

data ImportSpec = ImportSpec Bool Ident (Maybe Ident)
  deriving (Show)

data Expr
  = EVar Ident
  | EApp Expr Expr
  | ELam [Ident] Expr
  | EInt Integer
  | EChar Char
  | EStr String
  | ECase Expr [(EPat, Expr)]
  | ELet [EDef] Expr
  | ETuple [Expr]
  | EList [Expr]
  | EDo (Maybe Ident) [EStmt]
  | EPrim String
  | ESectL Expr Ident
  | ESectR Ident Expr
  | EIf Expr Expr Expr
  | ECompr Expr [EStmt]
  deriving (Show)

data EStmt = Bind Ident Expr | Then Expr | Let [EDef]
  deriving (Show)

data EPat
  = PConstr Ident [Ident]
  | PTuple [Ident]
  deriving (Show)

data EModule = EModule IdentModule [ExportSpec] [EDef]
  deriving (Show)

data ExportSpec = ExpModule IdentModule
  deriving (Show)

type LHS = (Ident, [Ident])
type Constr = (Ident, [Type])
type Type = Expr

qual :: Ident -> Ident -> Ident
qual qi i = qi ++ "." ++ i

skipWhite :: P a -> P a
skipWhite p = p <* pWhite

skipWhite' :: P a -> P a
skipWhite' p = p <* emany (char ' ')

-- Skip white-space.
-- If there is a newline, return the indentation of the last line.
pIndent :: P (Maybe String)
pIndent = do
  s <- emany (satisfy "white-space" (`elem` " \n\r"))
  let s' = takeWhile (== ' ') $ reverse s
  if s == s' then
    pure Nothing
   else
    pure $ Just s'

pWhite :: P ()
pWhite = do
  msp <- pIndent
  case msp of
    Nothing -> pure ()
    Just sp -> do
      st <- get
      case st of
        [] -> pure ()
        i : is -> do
          let
            c = length sp
          if c < i then do
            inject ('}' : '\n' : sp)
            put is
           else if c > i then
            pure ()
           else do
            eof <|> inject ";"

esepBy1 :: Prsr s a -> Prsr s sep -> Prsr s [a]
esepBy1 p sep = (:) <$> p <*> emany (sep *> p)

esepBy :: Prsr s a -> Prsr s sep -> Prsr s [a]
esepBy p sep = esepBy1 p sep <|> pure []

parseDie :: (Show a) => P a -> FilePath -> String -> a
parseDie p fn file =
  case runPrsr [] p fn (removeComments file) of
    Left err -> error err
    Right [(x, _)] -> x
    Right as -> error $ "Ambiguous:\n" ++ unlines (map (show . fst) as)

-- Remove comments first instead of having them in the parser.
removeComments :: String -> String
removeComments = unlines . map remCom . lines
  where remCom ('-':'-':_) = ""
        remCom ('"':cs) = '"':str cs
        remCom (c:cs) = c : remCom cs
        remCom "" = ""
        str ('"':cs) = '"' : remCom cs
        str ('\\':c:cs) = '\\':c:str cs
        str (c:cs) = c:str cs
        str "" = ""

-------

isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

isLetter :: Char -> Bool
isLetter c = isLower c || isUpper c

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

-------

pTop :: P EModule
pTop = skipWhite (pure ()) *> pModule <* eof

pModule :: P EModule
pModule = EModule <$> (pKeyword "module" *> pUIdent) <*>
                      (pSym '(' *> esepBy pExportSpec (pSym ',') <* pSym ')') <*>
                      (pKeyword' "where" *> pBlock pDef)

pExportSpec :: P ExportSpec
pExportSpec = ExpModule <$> (pKeyword "module" *> pUIdent)

pKeyword :: String -> P ()
pKeyword kw = (skipWhite $ do
  s <- pWord
  guard (kw == s)
  pure ()
  ) <?> kw

pKeyword' :: String -> P ()
pKeyword' kw = (skipWhite' $ do
  s <- pWord
  guard (kw == s)
  pure ()
  ) <?> kw

pLIdentA :: P String
pLIdentA = skipWhite $ do
  s <- pQIdent
  guard $ isLower $ head s
  pure s

pLIdent :: P String
pLIdent = pLIdentA <|> (pSym '(' *> pOperL <* pSym ')')

pLIdent_ :: P String
pLIdent_ = pLIdent <|> skipWhite (string "_")

pUIdentA :: P String
pUIdentA = skipWhite $ do
  s <- pQIdent
  guard $ isUpper $ head s
  pure s

pUIdent :: P String
pUIdent = pUIdentA <|> (pSym '(' *> pOperU <* pSym ')')

keywords :: [String]
keywords = ["case", "data", "do", "else", "if", "import",
  "in", "let", "module", "of", "primitive", "then", "where"]

pWord :: P String
pWord = (:) <$> satisfy "letter" isLetter <*>
                (emany $ satisfy "letter, digit" $ \ c ->
                    isLetter c || isDigit c ||
                    c == '_' || c == '\'')

pIdent :: P String
pIdent = do
  s <- pWord
  guard (s `notElem` keywords)
  pure s

pQIdent :: P String
pQIdent = intercalate "." <$> esepBy1 pIdent (char '.')

pInt :: P Integer
pInt = (read <$> (skipWhite $ esome $ satisfy "digit" isDigit)) <?> "int"

pChar :: P Char
pChar = skipWhite (char '\'' *> pc <* char '\'')
  where pc = do
          c <- satisfy "char" (/= '\'')
          if c == '\\' then
            decodeChar <$> satisfy "char" (const True)
           else
            pure c

pString :: P String
pString = skipWhite (char '"' *> emany pc <* char '"')
  where pc = do
          c <- satisfy "char" (/= '"')
          guard (c /= '\n')
          if c == '\\' then
            decodeChar <$> satisfy "char" (const True)
           else
            pure c

decodeChar :: Char -> Char
decodeChar 'n' = '\n'
decodeChar c = c

pSymbol :: String -> P ()
pSymbol s = (do
  s' <- pOper'
  guard (s == s')
  pure ()
  ) <?> s

pOper' :: P String
pOper' = skipWhite $ esome $ satisfy "symbol" (`elem` "@\\=+-:<>.!#$%^&*/|~?")

pOper :: P String
pOper = do
  s <- pOper'
  guard $ s `notElem` ["=", "|", "::", "<-", "@"]
  pure s

pOperL :: P String
pOperL = do
  s <- pOper
  guard $ head s /= ':'
  pure s

pOperU :: P String
pOperU = do
  s <- pOper
  guard $ head s == ':'
  pure s

pOpers :: [String] -> P String
pOpers ops = do
  op <- pOper
  guard (op `elem` ops)
  pure op

pSym :: Char -> P ()
pSym c = () <$ (skipWhite $ char c)

pLHS :: P Ident -> P LHS
pLHS pId = (,) <$> pId <*> many pLIdent_

pDef :: P EDef
pDef =
      Data   <$> (pKeyword "data" *> pLHS pUIdent <* pSymbol "=") <*> esepBy1 ((,) <$> pUIdent <*> many pAType) (pSymbol "|")
  <|> Fcn    <$> (pLHS pLIdent_ <* pSymbol "=") <*> pExpr
  <|> Sign   <$> (pLIdent <* pSymbol "::") <*> pType
  <|> Import <$> (pKeyword "import" *> pImportSpec)

pImportSpec :: P ImportSpec
pImportSpec = ImportSpec <$> pQua <*> pUIdent <*> optional (pKeyword "as" *> pUIdent)
  where pQua = (True <$ pKeyword "qualified") <|< pure False

pAType :: P Type
pAType = pAExpr

pType :: P Type
pType = pExpr

pAExpr :: P Expr
pAExpr =
      (EVar <$> pLIdent)
  <|> (EVar <$> pUIdent)
  <|> (EInt <$> pInt)
  <|> (EChar <$> pChar)
  <|> (EStr <$> pString)
  <|> (ETuple <$> (pSym '(' *> esepBy pExpr (pSym ',') <* pSym ')'))
  <|> (EList <$> (pSym '[' *> esepBy pExpr (pSym ',') <* pSym ']'))
  <|> (EPrim <$> (pKeyword "primitive" *> pString))
  <|> (ESectL <$> (pSym '(' *> pExprArg) <*> (pOper <* pSym ')'))
  <|> (ESectR <$> (pSym '(' *> pOper) <*> (pExprArg <* pSym ')'))
  <|> (ECompr <$> (pSym '[' *> pExpr <* pSym '|') <*> (esepBy1 pStmt (pSym ',') <* pSym ']'))

pExprApp :: P Expr
pExprApp = do
  f <- pAExpr
  as <- emany pAExpr
  pure $ foldl EApp f as

pLam :: P Expr
pLam = ELam <$> (pSymbol "\\" *> esome pLIdent_) <*> (pSymbol "->" *> pExpr)

pCase :: P Expr
pCase = ECase <$> (pKeyword "case" *> pExpr) <*> (pKeyword' "of" *> pBlock pArm)
  where pArm = (,) <$> (pPat <* pSymbol "->") <*> pExpr

pPat :: P EPat
pPat =
      ((uncurry PConstr) <$> pLHS pUIdent)
  <|> (PTuple <$> (pSym '(' *> esepBy pLIdent_ (pSym ',') <* pSym ')'))
  <|> (PConstr "Nil" [] <$ (pSym '[' <* pSym ']'))   -- Hack for [] = Nil
  <|> (PConstr "Unit" [] <$ (pSym '(' <* pSym ')'))   -- Hack for () = Unit
  <|> ((\ x s y -> PConstr s [x,y]) <$> pLIdent_ <*> pOperU <*> pLIdent_)

pLet :: P Expr
pLet = ELet <$> (pKeyword' "let" *> pBlock pDef) <*> (pKeyword "in" *> pExpr)

pExprOp :: P Expr
pExprOp = p0
  where
    p0 = pRightAssoc (pOpers ["$", "->"]) p1   -- XXX where should -> be?
    p1 = pLeftAssoc  (pOpers [">>=", ">>"]) p2
    p2 = pRightAssoc (pOpers ["||"]) p3
    p3 = pRightAssoc (pOpers ["&&"]) p4
    p4 = pNonAssoc   (pOpers ["==", "/=", "<", "<=", ">", ">="]) p5
    p5 = pRightAssoc (pOpers [":", "++"]) p6
    p6 = pLeftAssoc  (pOpers ["+", "-"]) p7
    p7 = pLeftAssoc  (pOpers ["*", "quot", "rem"]) p8
    p8 = p9
    p9 = pRightAssoc (pOpers ["."]) p10
    p10 = pExprArg

appOp :: String -> Expr -> Expr -> Expr
appOp op e1 e2 = EApp (EApp (EVar op) e1) e2

pRightAssoc :: P String -> P Expr -> P Expr
pRightAssoc pOp p = do
  e1 <- p
  let rest = do
        op <- pOp
        e2 <- pRightAssoc pOp p
        pure $ appOp op e1 e2
  rest <|< pure e1

pNonAssoc :: P String -> P Expr -> P Expr
pNonAssoc pOp p = do
  e1 <- p
  let rest = do
        op <- pOp
        e2 <- p
        pure $ appOp op e1 e2
  rest <|< pure e1

pLeftAssoc :: P String -> P Expr -> P Expr
pLeftAssoc pOp p = do
  e1 <- p
  es <- emany ((,) <$> pOp <*> p)
  pure $ foldl (\ x (op, y) -> appOp op x y) e1 es

pExprArg :: P Expr
pExprArg = pExprApp <|> pLam <|> pCase <|> pLet <|> pIf

pExpr :: P Expr
pExpr = pExprOp <|> pDo

pDo :: P Expr
pDo = EDo <$> ((Just <$> pQualDo) <|< (Nothing <$ pKeyword' "do")) <*> pBlock pStmt

pIf :: P Expr
pIf = EIf <$> (pKeyword "if" *> pExpr) <*> (pKeyword "then" *> pExpr) <*> (pKeyword "else" *> pExpr)

pStmt :: P EStmt
pStmt =
      (Bind <$> (pLIdent <* pSymbol "<-") <*> pExpr)
  <|> (Let  <$> (pKeyword' "let" *> pBlock pDef))
  <|> (Then <$> pExpr)

pQualDo :: P String
pQualDo = do
  s <- pUIdent
  _ <- char '.'
  pKeyword' "do"
  pure s

pLCurl :: P ()
pLCurl = pSym '{' <|< softLC
  where softLC = do
          msp <- pIndent
          case msp of
            Nothing -> fail "\\n"
            Just sp -> do
--              traceM ("push " ++ show (length sp))
              modify $ \ st -> length sp : st

pRCurl :: P ()
pRCurl = pSym '}' <|> softRC
  where softRC = do
          is <- get
          if null is then
            fail "}"
           else do
            put (tail is)
            eof

pBlock :: P a -> P [a]
pBlock p = do
  pLCurl
  as <- esepBy p (pSym ';')
  pRCurl
  pure as
