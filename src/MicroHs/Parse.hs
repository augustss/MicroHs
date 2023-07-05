{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}
{-# LANGUAGE QualifiedDo #-}
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
  EBind(..),
  EModule(..),
  ExportSpec(..),
  LHS,
  ) where
import Prelude hiding (Monad(..), Applicative(..), MonadFail(..), Functor(..), (<$>))
--import Control.Monad
--import Control.Monad.State.Strict
--import Control.Applicative --hiding (many, some)
import Data.Char
import Data.List
import Text.ParserComb as P
--import Debug.Trace


type P a = Prsr [Int] a

type Ident = String
type IdentModule = Ident

data EDef
  = Data LHS [Constr]
  | Type LHS Type
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
  | EInt Int
  | EChar Char
  | EStr String
  | ECase Expr [(EPat, Expr)]
  | ELet [EBind] Expr
  | ETuple [Expr]
  | EList [Expr]
  | EDo (Maybe Ident) [EStmt]
  | EPrim String
  | ESectL Expr Ident
  | ESectR Ident Expr
  | EIf Expr Expr Expr
  | ECompr Expr [EStmt]
  deriving (Show)

data EStmt = SBind Ident Expr | SThen Expr | SLet [EBind]
  deriving (Show)

data EBind = BFcn LHS Expr | BPat EPat Expr
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
pIndent = P.do
  s <- emany (satisfy "white-space" (`elem` " \n\r"))
  let s' = takeWhile (== ' ') $ reverse s
  if s == s' then
    pure Nothing
   else
    pure $ Just s'

pWhite :: P ()
pWhite = P.do
  msp <- pIndent
  case msp of
    Nothing -> pure ()
    Just sp -> P.do
      st <- get
      case st of
        [] -> pure ()
        i : is -> P.do
          let
            c = length sp
          if c < i then P.do
            inject ('}' : '\n' : sp)
            put is
           else if c > i then
            pure ()
           else P.do
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
removeComments =
  let
    remCom acs =
      case stripPrefix "--" acs of
        Just cs -> skipToNL cs
        Nothing ->
          case stripPrefix "{-" acs of
            Just cs -> skipBlock 1 cs
            Nothing ->
              case acs of
                [] -> ""
                c:cs ->
                  if c == '"' then '"' : str cs
                  else if c == '\'' then '\'' : chr cs
                  else c : remCom cs
    str acs =
      case acs of
        [] -> ""
        c:cs ->
          if c == '"' then '"' : remCom cs
          else if c == '\\' then '\\':head cs:str (tail cs)
          else c : str cs
    chr acs =
      case acs of
        [] -> ""
        c:cs ->
          if c == '\'' then '\'' : remCom cs
          else if c == '\\' then '\\':head cs:chr (tail cs)
          else c : chr cs
    skipToNL acs =
      case acs of
        [] -> ""
        c:cs ->
          if c == '\n' then '\n' : remCom cs
          else skipToNL cs
    skipBlock n acs =
      if n == 0 then
        remCom acs
      else
        case stripPrefix "{-" acs of
          Just cs -> skipBlock (n+1) cs
          Nothing ->
            case stripPrefix "-}" acs of
              Just cs -> skipBlock (n-1) cs
              Nothing ->
                case acs of
                  [] -> ""
                  c:cs -> if c == '\n' then '\n' : skipBlock n cs
                          else skipBlock n cs
  in  remCom
{-
        remCom ('-':'-':cs) = skipToNL cs
        remCom ('{':'-':cs) = skipBlock 1 cs
        remCom ('"':cs) = '"':str cs
        remCom ('\'':cs) = '\'':chr cs
        remCom (c:cs) = c : remCom cs
        remCom "" = ""
        str ('"':cs) = '"' : remCom cs
        str ('\\':c:cs) = '\\':c:str cs
        str (c:cs) = c:str cs
        str "" = ""
        chr ('\'':cs) = '\'' : remCom cs
        chr ('\\':c:cs) = '\\':c:chr cs
        chr (c:cs) = c:chr cs
        chr "" = ""
        skipToNL ('\n':cs) = '\n' : remCom cs
        skipToNL (_ : cs) = skipToNL cs
        skipToNL "" = ""
        skipBlock :: Int -> String -> String
        skipBlock 0 cs = remCom cs
        skipBlock n ('{':'-':cs) = skipBlock (n+1) cs
        skipBlock n ('-':'}':cs) = skipBlock (n-1) cs
        skipBlock n ('\n':cs) = '\n' : skipBlock n cs
        skipBlock n (_:cs) = skipBlock n cs
        skipBlock _ "" = ""
-}

-------
{-
isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

isLetter :: Char -> Bool
isLetter c = isLower c || isUpper c

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'
-}
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
pKeyword kw = (skipWhite $ P.do
  s <- pWord
  guard (kw == s)
  pure ()
  ) <?> kw

pKeyword' :: String -> P ()
pKeyword' kw = (skipWhite' $ P.do
  s <- pWord
  guard (kw == s)
  pure ()
  ) <?> kw

pLIdentA :: P String
pLIdentA = skipWhite $ P.do
  s <- pQIdent
  guard $ isLower $ head s
  pure s

pLIdent :: P String
pLIdent = pLIdentA <|> (pSym '(' *> pOperL <* pSym ')')

pLIdent_ :: P String
pLIdent_ = pLIdent <|> skipWhite (string "_")

pUIdentA :: P String
pUIdentA = skipWhite $ P.do
  s <- pQIdent
  guard $ isUpper $ head s
  pure s

pUIdent :: P String
pUIdent = pUIdentA <|> (pSym '(' *> pOperU <* pSym ')')

keywords :: [String]
keywords = ["case", "data", "do", "else", "if", "import",
  "in", "let", "module", "of", "primitive", "then", "type", "where"]

pWord :: P String
pWord = (:) <$> satisfy "letter" isLetter <*>
                (emany $ satisfy "letter, digit" $ \ c ->
                    isLetter c || isDigit c ||
                    c == '_' || c == '\'')

pIdent :: P String
pIdent = P.do
  s <- pWord
  guard (s `notElem` keywords)
  pure s

pQIdent :: P String
pQIdent = intercalate "." <$> esepBy1 pIdent (char '.')

pInt :: P Int
pInt = (read <$> (skipWhite $ esome $ satisfy "digit" isDigit)) <?> "int"

pChar :: P Char
pChar = skipWhite (char '\'' *> pc <* char '\'')
  where pc = P.do
          c <- satisfy "char" (/= '\'')
          if c == '\\' then
            decodeChar <$> satisfy "char" (const True)
           else
            pure c

pString :: P String
pString = skipWhite (char '"' *> emany pc <* char '"')
  where pc = P.do
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
pSymbol s = (P.do
  s' <- pOper'
  guard (s == s')
  pure ()
  ) <?> s

pOper' :: P String
pOper' = skipWhite $ esome $ satisfy "symbol" (`elem` "@\\=+-:<>.!#$%^&*/|~?")

pOper :: P String
pOper = P.do
  s <- pOper'
  guard $ s `notElem` ["=", "|", "::", "<-", "@"]
  pure s

pOperL :: P String
pOperL = P.do
  s <- pOper
  guard $ head s /= ':'
  pure s

pOperU :: P String
pOperU = P.do
  s <- pOper
  guard $ head s == ':'
  pure s

pOpers :: [String] -> P String
pOpers ops = P.do
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
  <|> Type   <$> (pKeyword "type" *> pLHS pUIdent <* pSymbol "=") <*> pType
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
pExprApp = P.do
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
pLet = ELet <$> (pKeyword' "let" *> pBlock pBind) <*> (pKeyword "in" *> pExpr)

pExprOp :: P Expr
pExprOp = p0
  where
    p0 = pRightAssoc (pOpers ["$", "->"]) p1   -- XXX where should -> be?
    p1 = pLeftAssoc  (pOpers [">>=", ">>"]) p2
    p2 = pRightAssoc (pOpers ["||"]) p3
    p3 = pRightAssoc (pOpers ["&&"]) $
         pLeftAssoc  (pOpers ["<|>","<|<"]) p4
    p4 = pNonAssoc   (pOpers ["==", "/=", "<", "<=", ">", ">="]) $
         pLeftAssoc  (pOpers ["<*>", "<*", "*>", "<$>", "<$"])   p5
    p5 = pRightAssoc (pOpers [":", "++"]) p6
    p6 = pLeftAssoc  (pOpers ["+", "-"]) p7
    p7 = pLeftAssoc  (pOpers ["*", "quot", "rem"]) p8
    p8 = p9
    p9 = pRightAssoc (pOpers ["."]) $
         pLeftAssoc  (pOpers ["?"]) p10
    p10 = pExprArg

appOp :: String -> Expr -> Expr -> Expr
appOp op e1 e2 = EApp (EApp (EVar op) e1) e2

pRightAssoc :: P String -> P Expr -> P Expr
pRightAssoc pOp p = P.do
  e1 <- p
  let rest = P.do
        op <- pOp
        e2 <- pRightAssoc pOp p
        pure $ appOp op e1 e2
  rest <|< pure e1

pNonAssoc :: P String -> P Expr -> P Expr
pNonAssoc pOp p = P.do
  e1 <- p
  let rest = P.do
        op <- pOp
        e2 <- p
        pure $ appOp op e1 e2
  rest <|< pure e1

pLeftAssoc :: P String -> P Expr -> P Expr
pLeftAssoc pOp p = P.do
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
      (SBind <$> (pLIdent <* pSymbol "<-") <*> pExpr)
  <|> (SLet  <$> (pKeyword' "let" *> pBlock pBind))
  <|> (SThen <$> pExpr)

pBind :: P EBind
pBind = 
      BFcn <$> (pLHS pLIdent_ <* pSymbol "=") <*> pExpr
  <|> BPat <$> (pPat <* pSymbol "=") <*> pExpr

pQualDo :: P String
pQualDo = P.do
  s <- pUIdent
  _ <- char '.'
  pKeyword' "do"
  pure s

pLCurl :: P ()
pLCurl = pSym '{' <|< softLC
  where softLC = P.do
          msp <- pIndent
          case msp of
            Nothing -> fail "\\n"
            Just sp -> P.do
--              traceM ("push " ++ show (length sp))
              modify $ \ st -> length sp : st

pRCurl :: P ()
pRCurl = pSym '}' <|> softRC
  where softRC = P.do
          is <- get
          if null is then
            fail "}"
           else P.do
            put (tail is)
            eof

pBlock :: P a -> P [a]
pBlock p = P.do
  pLCurl
  as <- esepBy p (pSym ';')
  pRCurl
  pure as
