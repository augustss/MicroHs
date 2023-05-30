module Parse(
  pTop,
  parseDie,
  Ident,
  Def(..),
  Expr(..),
  Pat(..),
  Module(..),
  ) where
import Control.Monad
import Control.Monad.State.Strict
import ParserComb
import Control.Applicative --hiding (many, some)
--import Debug.Trace

type P a = Prsr [Int] a

type Ident = String

data Def = Data LHS [Constr] | Fcn LHS Expr | Sign Ident Type
  deriving (Show)

data Expr
  = EVar Ident
  | EApp Expr Expr
  | ELam Ident Expr
  | EInt Integer
  | EChar Char
  | EStr String
  | ECase Expr [(Pat, Expr)]
  | ELet [Def] Expr
  | ETuple [Expr]
  | EList [Expr]
  deriving (Show)

data Pat
  = PConstr Ident [Ident]
  | PTuple [Ident]
  deriving (Show)

data Module = Module Ident [Def]
  deriving (Show)

type LHS = (Ident, [Ident])
type Constr = (Ident, [Type])
type Type = Expr

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
  xx <- restOfInput
  msp <- pIndent
--  traceM $ "pWhite " ++ show (xx, msp)
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
--            traceM ("inject " ++ show ('}':sp))
            inject ('}' : '\n' : sp)
            put is
           else if c > i then
            pure ()
           else do
            eof <|> do --traceM "inject ;"
                       inject ";"

esepBy1 :: Prsr s a -> Prsr s sep -> Prsr s [a]
esepBy1 p sep = do
  x <- p
  xs <- emany (sep *> p)
  return (x:xs)

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

pTop :: P Module
pTop = skipWhite (pure ()) *> pModule <* eof

pModule :: P Module
pModule = Module <$> (pKeyword "module" *> pUIdent <* pKeyword' "where") <*> pBlock pDef

pKeyword :: String -> P ()
pKeyword kw = (skipWhite $ do
  s <- pIdentRest
  guard (kw == s)
  pure ()
  ) <?> kw

pKeyword' :: String -> P ()
pKeyword' kw = (skipWhite' $ do
  s <- pIdentRest
  guard (kw == s)
  pure ()
  ) <?> kw

pLIdentA :: P String
pLIdentA = skipWhite $ do
  s <- (:) <$> (satisfy "lower case" (\ c -> 'a' <= c && c <= 'z')) <*> pIdentRest
  guard $ s `notElem` keywords
  pure s

pLIdent :: P String
pLIdent = pLIdentA <|> (pSym '(' *> pOper <* pSym ')')

pLIdent_ :: P String
pLIdent_ = pLIdent <|> skipWhite (string "_")

keywords :: [String]
keywords = ["case", "data", "in", "let", "module", "of", "where"]

pUIdent :: P String
pUIdent = skipWhite $
  (:) <$> (satisfy "upper case" (\ c -> 'A' <= c && c <= 'Z')) <*> pIdentRest

pIdentRest :: P String
pIdentRest = emany $ satisfy "letter, digit" $ \ c ->
  'a' <= c && c <= 'z' ||
  'A' <= c && c <= 'Z' ||
  '0' <= c && c <= '9' ||
  c == '_' || c == '\''

pInt :: P Integer
pInt = (read <$> (skipWhite $ esome $ satisfy "digit" (\ c -> '0' <= c && c <= '9'))) <?> "int"

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
pOper' = skipWhite $ esome $ satisfy "symbol" (`elem` "\\=+-:<>.!#$%^&*|~")

pOper :: P String
pOper = do
  s <- pOper'
  guard $ s `notElem` ["=", "|", "::"]
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

pDef :: P Def
pDef =
      Data <$> (pKeyword "data" *> pLHS pUIdent <* pSymbol "=") <*> esepBy1 ((,) <$> pUIdent <*> many pAType) (pSymbol "|")
  <|> Fcn  <$> (pLHS pLIdent_ <* pSymbol "=") <*> pExpr
  <|> Sign <$> (pLIdent <* pSymbol "::") <*> pType

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

pExprApp :: P Expr
pExprApp = do
  f <- pAExpr
  as <- emany pAExpr
  pure $ foldl EApp f as

pLam :: P Expr
pLam = ELam <$> (pSymbol "\\" *> pLIdent_) <*> (pSymbol "." *> pExpr)

pCase :: P Expr
pCase = ECase <$> (pKeyword "case" *> pExpr) <*> (pKeyword' "of" *> pBlock pArm)
  where pArm = (,) <$> (pPat <* pSymbol "->") <*> pExpr

pPat :: P Pat
pPat =
      ((uncurry PConstr) <$> pLHS pUIdent)
  <|> (PTuple <$> (pSym '(' *> esepBy pLIdent_ (pSym ',') <* pSym ')'))
  <|> (PConstr "[]" [] <$ (pSym '[' <* pSym ']'))
  <|> ((\ x y -> PConstr ":" [x,y]) <$> (pLIdent_ <* pSym ':') <*> pLIdent_)

pLet :: P Expr
pLet = ELet <$> (pKeyword' "let" *> pBlock pDef) <*> (pKeyword "in" *> pExpr)

pExprOp :: P Expr
pExprOp = p0
  where
    p0 = pRightAssoc (pOpers ["$", "->"]) p1   -- XXX where should -> be?
    p1 = p2
    p2 = pRightAssoc (pOpers ["||"]) p3
    p3 = pRightAssoc (pOpers ["&&"]) p4
    p4 = pNonAssoc   (pOpers ["==", "/=", "<", "<=", ">", ">="]) p5
    p5 = pRightAssoc (pOpers [":", "++"]) p6
    p6 = pLeftAssoc  (pOpers ["+", "-"]) p7
    p7 = pLeftAssoc  (pOpers ["*", "quot", "rem"]) p8
    p8 = p9
    p9 = pRightAssoc (pOpers ["."]) p10
    p10 = pExprApp

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

pExpr :: P Expr
pExpr =
  pExprOp <|> pLam <|> pCase <|> pLet

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

pSemi :: P ()
pSemi = pSym ';'

pBlock :: P a -> P [a]
pBlock p = do
  pLCurl
  as <- esepBy p pSemi
  pRCurl
  pure as
