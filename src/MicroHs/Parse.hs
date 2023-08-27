-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing -Wno-unused-do-bind #-}
module MicroHs.Parse(
  pTop, parseDie
  ) where
import Prelude --Xhiding (Monad(..), Applicative(..), MonadFail(..), Functor(..), (<$>), showString, showChar, showList)
--import Control.Monad
--import Control.Monad.State.Strict
--import Control.Applicative --hiding (many, some)
import Data.Char
import Data.List
import Text.ParserComb as P
--import Debug.Trace
import MicroHs.Expr
--Ximport Compat


type P a = Prsr [Int] Char a

skipWhite :: forall a . P a -> P a
skipWhite p = p <* pWhiteIndent

skipWhiteW :: forall a . P a -> P a
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

parseDie :: forall a . --X (Show a) =>
            P a -> String -> String -> a
parseDie p fn file =
  case runPrsr [] p (removeComments file) of
    Left lf -> error $ formatFailed fn file lf
    Right [(a, _)] -> a
    Right as -> error $ "Ambiguous:"
--X                     ++ unlines (map (show . fst) as)

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
pExportSpec =
      ExpModule <$> (pKeyword "module" *> pUIdent)
  <|> ExpTypeCon <$> (pUIdent <* pSym '(' <* pSymbol ".." <* pSym ')')
  <|> ExpType <$> pUIdent
  <|> ExpValue <$> pLIdent

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
    guard $ isLower_ $ head s
    pure s

pLIdent :: P String
pLIdent = pLIdentA <|> (pSym '(' *> pOperL <* pSym ')')

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
  <|> (pSym '(' *> some (char ',') <* pSym ')')
  <|> ("()" <$ (pSym '(' *> pWhite *> pSym ')'))  -- Allow () as a constructor name
  <|> ("[]" <$ (pSym '[' *> pWhite *> pSym ']'))  -- Allow [] as a constructor name

keywords :: [String]
keywords = ["case", "data", "do", "else", "forall", "if", "import",
  "in", "let", "module", "newtype", "of", "primitive", "then", "type", "where"]

isAlpha_ :: Char -> Bool
isAlpha_ c = eqChar c '_' || isAlpha c

isLower_ :: Char -> Bool
isLower_ c = eqChar c '_' || isLower c

pWord :: P String
pWord = (:) <$> satisfy "letter" isAlpha_ <*>
                (emany $ satisfy "letter, digit" $ \ c ->
                    isAlpha_ c || isDigit c ||
                    eqChar c '\'')

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
  else if eqChar c '\'' then
    '\''
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

pLHS :: P LHS
pLHS = pair <$> pUIdent <*> many pLIdent

pDef :: P EDef
pDef =
      Data        <$> (pKeyword "data"    *> pLHS <* pSym '=') <*> esepBy1 (pair <$> pUIdent <*> many pAType) (pSymbol "|")
  <|> Newtype     <$> (pKeyword "newtype" *> pLHS <* pSym '=') <*> pUIdent <*> pAType
  <|> Type        <$> (pKeyword "type"    *> pLHS <* pSym '=') <*> pType
  <|> uncurry Fcn <$> pEqns
  <|> Sign        <$> (pLIdent <* pSymbol "::") <*> pTypeScheme
  <|> Import      <$> (pKeyword "import" *> pImportSpec)

pEqns :: P (Ident, [Eqn])
pEqns = P.do
  (name, eqn@(Eqn ps _)) <- pEqn (\ _ _ -> True)
  neqns <- emany (pSym ';' *> pEqn (\ n l -> eqIdent n name && l == length ps))
  P.pure (name, eqn : map snd neqns)

pEqn :: (Ident -> Int -> Bool) -> P (Ident, Eqn)
pEqn test = P.do
  name <- pLIdent
  pats <- emany pAPat
  alts <- pAlts (pSymbol "=")
  guard (test name (length pats))
  P.pure (name, Eqn pats alts)

pImportSpec :: P ImportSpec
pImportSpec =
  let
    pQua = (True <$ pKeyword "qualified") <|< pure False
  in  ImportSpec <$> pQua <*> pUIdent <*> optional (pKeyword "as" *> pUIdent)

{-
pAType :: P EType
pAType = P.do
  t <- pAExprPT
  guard (validType t)
  pure t

pType :: P EType
pType = P.do
  t <- pExprPT
  guard (validType t)
  pure t
-}

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
      (EVar <$> pLIdent)
  <|> (EVar <$> pUIdent)
  <|> (ELit <$> pLit)
  <|> (eTuple <$> (pSym '(' *> esepBy1 pType (pSym ',') <* pSym ')'))
  <|> (EList . (:[]) <$> (pSym '[' *> pType <* pSym ']'))  -- Unlike expressions, only allow a single element.

------------------

pExpr :: P Expr
pExpr = P.do
  t <- pExprPT
  -- guard (validExpr t)
  pure t

pTypeScheme :: P ETypeScheme
pTypeScheme = P.do
  vs <- (pKeyword "forall" *> esome pLIdent <* pSym '.') <|< pure []
  t <- pType
  pure $ ETypeScheme vs t

pAExprPT :: P Expr
pAExprPT =
      (EVar <$> pLIdent)
  <|> (EVar <$> pUIdent)
  <|> (ELit <$> pLit)
  <|> (eTuple <$> (pSym '(' *> esepBy1 pExprPT (pSym ',') <* pSym ')'))
  <|> (EList <$> (pSym '[' *> esepBy1 pExprPT (pSym ',') <* pSym ']'))
  <|> (ELit . LPrim <$> (pKeyword "primitive" *> pString))
  <|> (ESectL <$> (pSym '(' *> pExprArg) <*> (pOper <* pSym ')'))
  <|> (ESectR <$> (pSym '(' *> pOper) <*> (pExprArg <* pSym ')'))
  <|> (ECompr <$> (pSym '[' *> pExprPT <* pSym '|') <*> (esepBy1 pStmt (pSym ',') <* pSym ']'))

pLit :: P Lit
pLit =
      (LInt <$> pInt)
  <|> (LChar <$> pChar)
  <|> (LStr <$> pString)

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
  f <- pAExprPT
  as <- emany pAExprPT
  pure $ foldl EApp f as

pLam :: P Expr
pLam = ELam <$> (pSymbol "\\" *> esome pAPat) <*> (pSymbol "->" *> pExprPT)

pCase :: P Expr
pCase = ECase <$> (pKeyword "case" *> pExprPT) <*> (pKeywordW "of" *> pBlock pCaseArm)

pCaseArm :: P ECaseArm
pCaseArm = pair <$> pPat <*> pAlts (pSymbol "->")

pAlts :: P () -> P EAlts
pAlts sep = P.do
  alts <- pAltsL sep
  bs <- pWhere
  P.pure (EAlts alts bs)
  
pAltsL :: P () -> P [EAlt]
pAltsL sep =
      esome (pair <$> (pSym '|' *> esepBy1 pStmt (pSym ',')) <*> (sep *> pExpr))
  <|< ((\ e -> [([], e)]) <$> (sep *> pExpr))

pWhere :: P [EBind]
pWhere =
      (pKeyword "where" *> pBlock pBind)
  <|< P.pure []

-- Sadly pattern and expression parsing cannot be joined because the
-- use of '->' in 'case' and lambda makes it weird.
-- Instead this is just a copy of some of the expression rules.
-- XXX This can probably be joined with pExpr again now that pType
-- is separate.
pAPat :: P EPat
pAPat =
      (EVar <$> pLIdent)
  <|> (EVar <$> pUIdent)
  <|> (ELit <$> pLit)
  <|> (eTuple <$> (pSym '(' *> esepBy1 pPat (pSym ',') <* pSym ')'))
  <|> (EList <$> (pSym '[' *> esepBy1 pPat (pSym ',') <* pSym ']'))
  <|> (EAt <$> (pLIdent <* pSymbol "@") <*> pAPat)

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

pLet :: P Expr
pLet = ELet <$> (pKeywordW "let" *> pBlock pBind) <*> (pKeyword "in" *> pExprPT)

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
    p0 = pRightAssoc (pOpers ["$"]) p1
  in  p0

-- A hack so that the . operator is not followed by a letter
pDot :: P String
pDot = skipWhite $ P.do
  char '.'
  notFollowedBy (satisfy "not alpha" isAlpha_)
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

pExprPT :: P Expr
pExprPT = pExprOp

pDo :: P Expr
pDo = EDo <$> ((Just <$> pQualDo) <|< (Nothing <$ pKeywordW "do")) <*> pBlock pStmt

pIf :: P Expr
pIf = EIf <$> (pKeyword "if" *> pExprPT) <*> (pKeyword "then" *> pExprPT) <*> (pKeyword "else" *> pExprPT)

pStmt :: P EStmt
pStmt =
      (SBind <$> (pPat <* pSymbol "<-") <*> pExprPT)
  <|> (SLet  <$> (pKeywordW "let" *> pBlock pBind))
  <|> (SThen <$> pExprPT)

pBind :: P EBind
pBind = 
      uncurry BFcn <$> pEqns
  <|> BPat <$> (pPatNotVar <* pSym '=') <*> pExprPT

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

pBlock :: forall a . P a -> P [a]
pBlock p = P.do
  pLCurl
  as <- esepBy p (pSym ';')
  pRCurl
  pure as

--------------

----------------

formatFailed :: String -> String -> LastFail Char -> String
formatFailed fn file lf =
  case lf of
    LastFail len _ _ ->
      let
        (pre, post) = splitAt (length file - len) file
        count lc x =
          case lc of
            (l, c) ->
              if eqChar x '\n' then (l+1, 0) else (l, c+1)
        (line, col) = foldl count (1, 0) pre
      in showString fn ++ ": " ++
         "line " ++ showInt line ++ ", col " ++ showInt col ++ ":\n" ++
         "   found: " ++ showString (take 10 post)
{-
    xs' = nub $ map trim xs
    pr e = "   expeced: " ++ e
    trim arg = unwords (snd arg) -- (last $ init $ "" : "" : es)
  in  show fn ++ ": " ++
      "line " ++ show line ++ ", col " ++ show col ++ ":\n" ++
      "   found: " ++ show (takeWhile (not . isSpace) post) ++ "\n" ++
      unlines (map pr xs')
-}

char :: forall s . Char -> Prsr s Char Char
char c = satisfy "char" (eqChar c)
