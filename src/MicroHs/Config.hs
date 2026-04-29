{-# OPTIONS_GHC -Wno-orphans #-}
module MicroHs.Config(
    Key, Value, SectionName, Section, Config
  , parseConfig
  ) where
import qualified Prelude(); import MHSPrelude hiding(lex)
import Data.List
import Text.ParserComb
import MicroHs.Ident
import MicroHs.Lex

{- File Format
[section_name]
key = "value"
...

[next_section]
key = ...
-}

type Key = String
type Value = String
type SectionName = String
type Section = (SectionName, [(Key, Value)])
type Config = [Section]

-- Parser

type Parser = Prsr [Token] Token

instance TokenMachine [Token] Token where
  tmNextToken [] = (TEnd noSLoc, [])
  tmNextToken (x:xs) = (x,xs)

  tmRawTokens = id


eof :: Parser ()
eof = do
  t <- nextToken
  case t of
    TEnd _ -> pure ()
    _      -> fail "eof"

nl :: Parser [Token]
nl = many $ satisfy "\\n" isWhite
  where isWhite (TIndent _) = True
        isWhite _           = False

spec :: Char -> Parser Token
spec c = satisfy (showToken $ TSpec (SLoc "" 0 0) c) is
  where is (TSpec _ d) = c == d
        is _ = False

ident :: Parser String
ident = satisfyM "key" is
  where is (TIdent _ _ x) = Just x
        is _              = Nothing

key :: Parser Key
key = ident

value :: Parser Value
value = satisfyM "value" isValue
  where isValue (TString _ x) = Just x
        isValue _             = Nothing

sectionName :: Parser SectionName
sectionName = spec '[' *> ident <* spec ']'

keyValue :: Parser (String, String)
keyValue = (,) <$> key <*> (spec '=' *> value)

keyValues :: Parser [(Key, Value)]
keyValues = keyValue `sepBy` nl

section :: Parser Section
section = (,) <$> (sectionName <* nl) <*> keyValues

sections :: Parser [Section]
sections = nl *> (section `sepBy1` nl) <* nl <* eof

formatFailed :: LastFail Token -> String
formatFailed (LastFail _ ts msgs) =
  unlines [ showSLoc sloc ++ ":\n"
          , "  found: " ++ head (map showToken ts)
          , "  expected: " ++ unwords (nub msgs)
          ]
  where sloc = tokensLoc ts

parseConfig :: FilePath -> String -> Either String Config
parseConfig fp file =
  case runPrsr sections $ lex (SLoc fp 1 1) file of
    Left lf -> Left $ formatFailed lf
    Right a -> Right a
