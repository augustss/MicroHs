module MicroHs.TargetConfig
( Target(..)
, TTarget(..)
, parseTargets
, findTarget
) where

import Text.ParserComb
import MicroHs.Ident
import MicroHs.Lex
import Data.List
import Control.Applicative
import Prelude hiding(lex)

{- File Format
[target_name]
key = "value"
...

[next_target]
key = ...
-}

data Target = Target String [(String,String)]
  deriving Show

data TTarget = TTarget
  { tName :: String
  , tCC   :: String
  , tConf :: String
  }

findTarget :: String -> [Target] -> Maybe Target
findTarget _ [] = Nothing
findTarget name ((Target n conf):ts) | n == name = Just $ Target n conf
                                     | otherwise = findTarget name ts


-- Parser


type Parser = Prsr [Token] Token

instance TokenMachine [Token] Token where
  tmNextToken [] = (TEnd, [])
  tmNextToken (x:xs) = (x,xs)

  tmRawTokens = id


eof :: Parser ()
eof = do
  t <- nextToken
  case t of
    TEnd -> pure ()
    _    -> fail "eof"

nl :: Parser [Token]
nl = many $ satisfy "\\n" isWhite
  where isWhite (TIndent _) = True
        isWhite _           = False

obrac :: Parser Token
obrac = satisfy "[" isOBrac
  where isOBrac (TSpec _ '[') = True
        isOBrac _             = False

cbrac :: Parser Token
cbrac = satisfy "]" isCBrac
  where isCBrac (TSpec _ ']') = True
        isCBrac _             = False

eq :: Parser Token
eq = satisfy "=" isEq
  where isEq (TIdent _ _ "=") = True
        isEq _             = False

key :: Parser String
key = satisfyM "key" isKey
  where isKey (TIdent _ _ x) = Just x
        isKey _              = Nothing

value :: Parser String
value = satisfyM "value" isValue
  where isValue (TString _ x) = Just x
        isValue _             = Nothing

targetName :: Parser String
targetName = obrac *> key <* cbrac

keyValue :: Parser (String,String)
keyValue = do
  k <- key
  _ <- eq
  v <- value
  return (k,v)

keyValues :: Parser [(String,String)]
keyValues = keyValue `esepBy` nl


target :: Parser Target
target = liftA2 Target (targetName <* nl) keyValues

targets :: Parser [Target]
targets = (target `sepBy1` nl) <* nl <* eof

formatFailed :: LastFail Token -> String
formatFailed (LastFail _ ts msgs) = unlines [ showSLoc sloc ++ ":\n"
                                            , "  found: " ++ head (map showToken ts)
                                            , "  expected: " ++ unwords (nub msgs)
                                            ]
  where sloc = tokensLoc ts

parseTargets :: FilePath -> String -> Either String [Target]
parseTargets fp file = case runPrsr targets $ lex (SLoc fp 1 1) file of
                         Left lf -> Left $ formatFailed lf
                         Right [a] -> Right a
                         Right as  -> Left $ "Ambiguous:" ++ unlines (map show as)
