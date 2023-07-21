-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing -Wno-unused-do-bind #-}
{-# LANGUAGE QualifiedDo #-}
module MicroHs.Parse(
  module MicroHs.Parse
{-
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
-}
  ) where
import Prelude --Xhiding (Monad(..), Applicative(..), MonadFail(..), Functor(..), (<$>), showString, showChar, showList)
--import Control.Monad
--import Control.Monad.State.Strict
--import Control.Applicative --hiding (many, some)
import Data.Char
import Data.List
import Text.ParserComb as P
--import Debug.Trace
--Ximport Compat


type P a = Prsr [Int] a

type Ident = String
type IdentModule = Ident

data EDef
  = Data LHS [Constr]
  | Type LHS EType
  | Fcn LHS Expr
  | Sign Ident ETypeScheme
  | Import ImportSpec
  --Xderiving (Show)

data ImportSpec = ImportSpec Bool Ident (Maybe Ident)
  --Xderiving (Show)

data Expr
  = EVar Ident
  | EApp Expr Expr
  | ELam [Ident] Expr
  | EInt Int
  | EChar Char
  | EStr String
  | ECase Expr [ECaseArm]
  | ELet [EBind] Expr
  | ETuple [Expr]
  | EList [Expr]
  | EDo (Maybe Ident) [EStmt]
  | EPrim String
  | ESectL Expr Ident
  | ESectR Ident Expr
  | EIf Expr Expr Expr
  | ECompr Expr [EStmt]
  | EBad
  --Xderiving (Show)

type ECaseArm = (EPat, Expr)

data EStmt = SBind EPat Expr | SThen Expr | SLet [EBind]
  --Xderiving (Show)

data EBind = BFcn LHS Expr | BPat EPat Expr
  --Xderiving (Show)

data EPat
  = PConstr Ident [EPat]
  | PVar Ident
  --Xderiving (Show)

isPVar :: EPat -> Bool
isPVar p =
  case p of
    PConstr _ _ -> False
    PVar _ -> True

data EModule = EModule IdentModule [ExportSpec] [EDef]
  --Xderiving (Show)

data ExportSpec = ExpModule IdentModule
  --Xderiving (Show)

type LHS = (Ident, [Ident])
type Constr = (Ident, [EType])

-- Expr restricted to
--  * after desugaring: EApp and EVar
--  * before desugaring: EApp, EVar, ETuple, EList
type EType = Expr

validType :: Expr -> Bool
validType ae =
  case ae of
    EVar _ -> True
    EApp f a -> validType f && validType a
    EList es -> length es <= 1 && all validType (take 1 es)
    ETuple es -> all validType es
    _ -> False

data ETypeScheme = ETypeScheme [Ident] EType
  --Xderiving (Show)

eqIdent :: Ident -> Ident -> Bool
eqIdent = eqString

leIdent :: Ident -> Ident -> Bool
leIdent = leString

showIdent :: Ident -> String
showIdent i = i

tupleConstr :: Int -> Ident
tupleConstr n = replicate (n-1) ','

untupleConstr :: Ident -> Int
untupleConstr s = length s + 1

---------------------------------

qual :: Ident -> Ident -> Ident
qual qi i = qi ++ "." ++ i

skipWhite :: P a -> P a
skipWhite p = p <* pWhiteIndent

skipWhiteW :: P a -> P a
skipWhiteW p = p <* emany (char ' ')

pWhite :: P String
pWhite = emany (satisfy "white-space" (\ c -> elemBy eqChar c " \n\r"))

-- Skip white-space.
-- If there is a newline, return the indentation of the last line.
pIndent :: P (Maybe String)
pIndent = P.do
  s <- pWhite
  let
    ss = takeWhile (eqChar ' ') $ reverse s
  if eqString s ss then
    pure Nothing
   else
    pure $ Just ss

pWhiteIndent :: P ()
pWhiteIndent = P.do
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
esepBy p sep = esepBy1 p sep <|< pure []

--XparseDie :: (Show a) => P a -> FilePath -> String -> a
parseDie p fn file =
  case runPrsr [] p fn (removeComments file) of
    Left err -> error err
    Right as ->
-- XXX parsing foo $ do ... is ambiguous
      if length as == 1 then
        fst (head as)
      else
        error $ "Ambiguous:"
--                 ++ unlines (map (show . fst) as)

-- Remove comments first instead of having them in the parser.
removeComments :: String -> String
removeComments = remCom

remCom :: [Char] -> [Char]
remCom acs =
  case stripPrefixBy eqChar "--" acs of
    Just cs -> skipToNL cs
    Nothing ->
      case stripPrefixBy eqChar "{-" acs of
        Just cs -> skipBlock 1 cs
        Nothing ->
          case acs of
            [] -> ""
            c:cs ->
              if eqChar c '"' then '"' : getStr cs
              else if eqChar c '\'' then '\'' : getChr cs
              else c : remCom cs

getStr :: [Char] -> [Char]
getStr acs =
  case acs of
    [] -> ""
    c:cs ->
      if eqChar c '"' then '"' : remCom cs
      else if eqChar c '\\' then '\\':head cs:getStr (tail cs)
      else c : getStr cs

getChr :: [Char] -> [Char]
getChr acs =
  case acs of
    [] -> ""
    c:cs ->
      if eqChar c '\'' then '\'' : remCom cs
      else if eqChar c '\\' then '\\':head cs:getChr (tail cs)
      else c : getChr cs

skipToNL :: [Char] -> [Char]
skipToNL acs =
  case acs of
    [] -> ""
    c:cs ->
      if eqChar c '\n' then '\n' : remCom cs
      else skipToNL cs

skipBlock :: Int -> [Char] -> [Char]
skipBlock n acs =
  if n == 0 then
    remCom acs
  else
    case stripPrefixBy eqChar "{-" acs of
      Just cs -> skipBlock (n+1) cs
      Nothing ->
        case stripPrefixBy eqChar "-}" acs of
          Just cs -> skipBlock (n-1) cs
          Nothing ->
            case acs of
              [] -> ""
              c:cs -> if eqChar c '\n' then '\n' : skipBlock n cs
                      else skipBlock n cs
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

pTop :: P EModule
pTop = skipWhite (pure ()) *> pModule <* eof

pModule :: P EModule
pModule = EModule <$> (pKeyword "module" *> pUIdent) <*>
                      (pSym '(' *> esepBy pExportSpec (pSym ',') <* pSym ')') <*>
                      (pKeywordW "where" *> pBlock pDef)

pExportSpec :: P ExportSpec
pExportSpec = ExpModule <$> (pKeyword "module" *> pUIdent)

pKeyword :: String -> P ()
pKeyword kw = skipWhite $
  P.do
    s <- pWord
    guard (eqString kw s)
    pure ()

pKeywordW :: String -> P ()
pKeywordW kw = skipWhiteW $
  P.do
    s <- pWord
    guard (eqString kw s)
    pure ()

pLIdentA :: P String
pLIdentA = skipWhite $
  P.do
    s <- pQIdent
    guard $ isLower $ head s
    pure s

pLIdent :: P String
pLIdent = pLIdentA <|> (pSym '(' *> pOperL <* pSym ')')

pLIdent_ :: P String
pLIdent_ = pLIdent <|> skipWhite (string "_")

pUIdentA :: P String
pUIdentA = skipWhite $
  P.do
    s <- pQIdent
    guard $ isUpper $ head s
    pure s

pUIdent :: P String
pUIdent =
      pUIdentA
  <|> (pSym '(' *> pOperU <* pSym ')')
  <|> ("()" <$ (pSym '(' *> pWhite *> pSym ')'))  -- Allow () as a constructor name
  <|> ("[]" <$ (pSym '[' *> pWhite *> pSym ']'))  -- Allow [] as a constructor name

keywords :: [String]
keywords = ["case", "data", "do", "else", "forall", "if", "import",
  "in", "let", "module", "of", "primitive", "then", "type", "where"]

pWord :: P String
pWord = (:) <$> satisfy "letter" isAlpha <*>
                (emany $ satisfy "letter, digit" $ \ c ->
                    isAlpha c || isDigit c ||
                    eqChar c '_' || eqChar c '\'')

pIdent :: P String
pIdent = P.do
  s <- pWord
  guard (not (elemBy eqString s keywords))
  pure s

pQIdent :: P String
pQIdent = intercalate "." <$> esepBy1 pIdent (char '.')

pInt :: P Int
pInt = readInt <$> (skipWhite $ esome $ satisfy "digit" isDigit)

pChar :: P Char
pChar =
  let
    pc =
      P.do
        c <- satisfy "char" (neChar '\'')
        if eqChar c '\\' then
          decodeChar <$> satisfy "char" (const True)
         else
          pure c
  in  skipWhite (char '\'' *> pc <* char '\'')

pString :: P String
pString =
  let
    pc =
      P.do
        c <- satisfy "char" (neChar '"')
        guard (neChar c '\n')
        if eqChar c '\\' then
          decodeChar <$> satisfy "char" (const True)
         else
          pure c
  in  skipWhite (char '"' *> emany pc <* char '"')

decodeChar :: Char -> Char
decodeChar c =
  if eqChar c 'n' then
    '\n'
  else if eqChar c 'r' then
    '\r'
  else if eqChar c 't' then
    '\t'
  else if eqChar c '\\' then
    '\\'
  else if eqChar c '\'' then
    '\''
  else if eqChar c '"' then
    '"'
  else
    error $ "decodeChar: " ++ showChar c

pSymbol :: String -> P ()
pSymbol s = P.do
  ss <- pOperW
  guard (eqString s ss)
  pure ()

pOperW :: P String
pOperW = skipWhite $ esome $ satisfy "symbol" (\x -> elemBy eqChar x "@\\=+-:<>.!#$%^&*/|~?")

pOper :: P String
pOper = P.do
  s <- pOperW
  guard $ not $ elemBy eqString s ["=", "|", "::", "<-", "@"]
  pure s

pOperL :: P String
pOperL = P.do
  s <- pOper
  guard $ neChar (head s) ':'
  pure s

pOperU :: P String
pOperU = P.do
  s <- pOper
  guard $ eqChar (head s) ':'
  pure s

pOpers :: [String] -> P String
pOpers ops = P.do
  op <- pOper
  guard (elemBy eqString op ops)
  pure op

pSym :: Char -> P ()
pSym c = () <$ (skipWhite $ char c)

pLHS :: P Ident -> P LHS
pLHS pId = pair <$> pId <*> many pLIdent_

pDef :: P EDef
pDef =
      Data   <$> (pKeyword "data" *> pLHS pUIdent <* pSymbol "=") <*> esepBy1 (pair <$> pUIdent <*> many pAType) (pSymbol "|")
  <|> Type   <$> (pKeyword "type" *> pLHS pUIdent <* pSymbol "=") <*> pType
  <|> Fcn    <$> (pLHS pLIdent_ <* pSymbol "=") <*> pExpr
  <|> Sign   <$> (pLIdent <* pSymbol "::") <*> pTypeScheme
  <|> Import <$> (pKeyword "import" *> pImportSpec)

pImportSpec :: P ImportSpec
pImportSpec =
  let
    pQua = (True <$ pKeyword "qualified") <|< pure False
  in  ImportSpec <$> pQua <*> pUIdent <*> optional (pKeyword "as" *> pUIdent)

pAType :: P EType
pAType = P.do
  t <- pAExpr
  guard (validType t)
  pure t

pType :: P EType
pType = P.do
  t <- pExpr
  guard (validType t)
  pure t

pTypeScheme :: P ETypeScheme
pTypeScheme = P.do
  vs <- (pKeyword "forall" *> esome pLIdent <* pSym '.') <|< pure []
  t <- pType
  pure $ ETypeScheme vs t

pAExpr :: P Expr
pAExpr =
      (EVar <$> pLIdent)
  <|> (EVar <$> pUIdent)
  <|> (EInt <$> pInt)
  <|> (EChar <$> pChar)
  <|> (EStr <$> pString)
  <|> (eTuple <$> (pSym '(' *> esepBy1 pExpr (pSym ',') <* pSym ')'))
  <|> (EList <$> (pSym '[' *> esepBy1 pExpr (pSym ',') <* pSym ']'))
  <|> (EPrim <$> (pKeyword "primitive" *> pString))
  <|> (ESectL <$> (pSym '(' *> pExprArg) <*> (pOper <* pSym ')'))
  <|> (ESectR <$> (pSym '(' *> pOper) <*> (pExprArg <* pSym ')'))
  <|> (ECompr <$> (pSym '[' *> pExpr <* pSym '|') <*> (esepBy1 pStmt (pSym ',') <* pSym ']'))

eTuple :: [Expr] -> Expr
eTuple aes =
  case aes of
    [] -> undefined
    e:es ->
      case es of
        [] -> e
        _ -> ETuple aes

pExprApp :: P Expr
pExprApp = P.do
  f <- pAExpr
  as <- emany pAExpr
  pure $ foldl EApp f as

pLam :: P Expr
pLam = ELam <$> (pSymbol "\\" *> esome pLIdent_) <*> (pSymbol "->" *> pExpr)

pCase :: P Expr
pCase =
  let
    pArm = pair <$> (pPat <* pSymbol "->") <*> pExpr
  in  ECase <$> (pKeyword "case" *> pExpr) <*> (pKeywordW "of" *> pBlock pArm)

pAPat :: P EPat
pAPat =
      (PVar <$> pLIdent_)
  <|> (cTuple <$> (pSym '(' *> esepBy1 pPat (pSym ',') <* pSym ')'))
  <|> (PConstr <$> pUIdent <*> pure [])

pPat :: P EPat
pPat =
      pAPat
  <|> (PConstr <$> pUIdent <*> esome pAPat)
  <|> ((\ x s y -> PConstr s [x,y]) <$> pAPat <*> pOperU <*> pAPat)

pPatC :: P EPat
pPatC = P.do
  p <- pPat
  guard (not (isPVar p))
  pure p

cTuple :: [EPat] -> EPat
cTuple aps =
  case aps of
    [] -> undefined
    p:ps ->
      case ps of
        [] -> p
        _ -> PConstr (tupleConstr (length aps)) aps

pLet :: P Expr
pLet = ELet <$> (pKeywordW "let" *> pBlock pBind) <*> (pKeyword "in" *> pExpr)

pExprOp :: P Expr
pExprOp =
  let
    p10 = pExprArg
    p9 = --pRightAssoc (pOpers ["."]) $
         pRightAssoc (pDot) $
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
    p0 = pRightAssoc (pOpers ["$", "->"]) p1   -- XXX where should -> be?
  in  p0

-- A hack so that the . operator is not followed by a letter
pDot :: P String
pDot = skipWhite $ P.do
  char '.'
  notFollowedBy (satisfy "not alpha" isAlpha)
  pure "."

appOp :: String -> Expr -> Expr -> Expr
appOp op e1 e2 = EApp (EApp (EVar op) e1) e2

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
  pure $ foldl (\ x opy -> appOp (fst opy) x (snd opy)) e1 es

pExprArg :: P Expr
pExprArg = pExprApp <|> pLam <|> pCase <|> pLet <|> pIf <|> pDo

pExpr :: P Expr
pExpr = pExprOp

pDo :: P Expr
pDo = EDo <$> ((Just <$> pQualDo) <|< (Nothing <$ pKeywordW "do")) <*> pBlock pStmt

pIf :: P Expr
pIf = EIf <$> (pKeyword "if" *> pExpr) <*> (pKeyword "then" *> pExpr) <*> (pKeyword "else" *> pExpr)

pStmt :: P EStmt
pStmt =
      (SBind <$> (pPat <* pSymbol "<-") <*> pExpr)
  <|> (SLet  <$> (pKeywordW "let" *> pBlock pBind))
  <|> (SThen <$> pExpr)

pBind :: P EBind
pBind = 
      BFcn <$> (pLHS pLIdent_ <* pSymbol "=") <*> pExpr
  <|> BPat <$> (pPatC <* pSymbol "=") <*> pExpr

pQualDo :: P String
pQualDo = P.do
  s <- pUIdent
  char '.'
  pKeywordW "do"
  pure s

pLCurl :: P ()
pLCurl =
  let
    softLC =
      P.do
        msp <- pIndent
        case msp of
          Nothing -> fail "\\n"
          Just sp -> P.do
--            traceM ("push " ++ show (length sp))
            modify $ \ st -> length sp : st
  in  pSym '{' <|< softLC

pRCurl :: P ()
pRCurl =
  let
    softRC =
      P.do
        is <- get
        if null is then
          fail "}"
         else P.do
          put (tail is)
          eof
  in  pSym '}' <|> softRC

pBlock :: P a -> P [a]
pBlock p = P.do
  pLCurl
  as <- esepBy p (pSym ';')
  pRCurl
  pure as

--------------

showEModule :: EModule -> String
showEModule am =
  case am of
    EModule i es ds -> "module " ++ i ++ "(\n" ++
      unlines (intersperse "," (map showExportSpec es)) ++
      "\n) where\n" ++
      showEDefs ds

showExportSpec :: ExportSpec -> String
showExportSpec ae =
  case ae of
    ExpModule i -> "module " ++ i

showEDef :: EDef -> String
showEDef def =
  case def of
    Fcn (f, vs) e -> unwords (f : vs) ++ " = " ++ showExpr e
    Sign i t -> i ++ " :: " ++ showETypeScheme t
    _ -> "<<EDef>>"

showEDefs :: [EDef] -> String
showEDefs ds = unlines (map showEDef ds)

showExpr :: Expr -> String
showExpr ae =
  case ae of
    EVar v -> v
    EApp f a -> "(" ++ showExpr f ++ " " ++ showExpr a ++ ")"
    ELam is e -> "(\\" ++ unwords is ++ " -> " ++ showExpr e ++ ")"
    EInt i -> showInt i
    EChar c -> showChar c
    EStr s -> showString s
    ECase _ _ -> "ECase"
    ELet _ _ -> "ELet"
    ETuple es -> "(" ++ intercalate "," (map showExpr es) ++ ")"
    EList es -> showList showExpr es
    EDo _ _ -> "EDo"
    EPrim p -> p
    ESectL e i -> "(" ++ showExpr e ++ " " ++ i ++ ")"
    ESectR i e -> "(" ++ i ++ " " ++ showExpr e ++ ")"
    EIf e1 e2 e3 -> "if " ++ showExpr e1 ++ " then " ++ showExpr e2 ++ " else " ++ showExpr e3
    ECompr _ _ -> "ECompr"
    EBad -> "EBad"

showEType :: EType -> String
showEType = showEType

showETypeScheme :: ETypeScheme -> String
showETypeScheme ts =
  case ts of
    ETypeScheme vs t ->
      if null vs
      then showEType t
      else unwords ("forall" : vs ++ [".", showEType t])
