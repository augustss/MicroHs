module HeadSymbolEq where

import Data.Proxy
import Data.TypeLits (Symbol, KnownSymbol, symbolVal, ConcatSymbol, HeadSymbol, SymbolEq)

--------------------------------------------------------------------------------
-- Specifier : Lit wraps a Symbol.
--------------------------------------------------------------------------------

data D -- digit
data S -- string
data Lit (lit :: Symbol)

class Specifier s
instance Specifier D
instance Specifier S
instance (KnownSymbol lit) => Specifier (Lit lit)

--------------------------------------------------------------------------------
-- FList : lists the formats.
--------------------------------------------------------------------------------

data FNil
data FCons s fl

class FList fl
instance FList FNil
instance (Specifier s, FList fl) => FList (FCons s fl)

--------------------------------------------------------------------------------
-- FormatF : splits the %d / %s formats
--------------------------------------------------------------------------------

class (FList format) => FormatF format fun | format -> fun where
  formatF :: Proxy format -> String -> fun

instance FormatF FNil String where
  formatF _ = id

instance (FormatF rest fun)
  => FormatF (FCons D rest) (Int -> fun) where
  formatF _ str = \i -> formatF (Proxy :: Proxy rest) (str ++ show i)

instance (FormatF rest fun)
  => FormatF (FCons S rest) (String -> fun) where
  formatF _ str = \s -> formatF (Proxy :: Proxy rest) (str ++ s)

instance (KnownSymbol lit, FormatF rest fun)
  => FormatF (FCons (Lit lit) rest) fun where
  formatF _ str
    = formatF (Proxy :: Proxy rest) (str ++ symbolVal (Proxy :: Proxy lit))

--------------------------------------------------------------------------------
-- MatchFmt
--------------------------------------------------------------------------------

class (Specifier out) => MatchFmt (head :: Symbol) out | head -> out
instance MatchFmt "d" D
instance MatchFmt "s" S

--------------------------------------------------------------------------------
-- Parse : uses SymbolEq -> "True"/"False" to avoid instance overlappings.
--------------------------------------------------------------------------------

class (FList format) => Parse (string :: Symbol) format | string -> format
instance (SymbolEq string "" isEmpty, ParseC isEmpty string format)
  => Parse string format

class (FList out) => ParseC (isEmpty :: Symbol) (string :: Symbol) out | isEmpty string -> out

instance ParseC "True" string (FCons (Lit "") FNil)

instance (HeadSymbol h t string, Match h t out)
  => ParseC "False" string out

--------------------------------------------------------------------------------
-- Match  : uses SymbolEq also.
--------------------------------------------------------------------------------

class (FList out) => Match (h :: Symbol) (t :: Symbol) out | h t -> out
instance (SymbolEq h "%" isPct, MatchC isPct h t out)
  => Match h t out

class (FList out) => MatchC (isPct :: Symbol) (h :: Symbol) (t :: Symbol) out
  | isPct h t -> out

-- '%' : on decompose t pour recuperer le caractere de specification (h2)
-- et le reste (t2)
instance (HeadSymbol h2 t2 t, MatchFmt h2 spec, Parse t2 rest)
  => MatchC "True" h t (FCons (Lit "") (FCons spec rest))

-- caractere ordinaire : accumule via ConcatSymbol (h prefixe acc)
instance (FList r, KnownSymbol acc', ConcatSymbol h acc acc', Parse t (FCons (Lit acc) r))
  => MatchC "False" h t (FCons (Lit acc') r)

--------------------------------------------------------------------------------
-- Format
--------------------------------------------------------------------------------

class Format (string :: Symbol) fun | string -> fun where
  format :: Proxy string -> fun

instance (Parse string format, FormatF format fun)
  => Format string fun where
  format _ = formatF (Proxy :: Proxy format) ""

--------------------------------------------------------------------------------
-- Exemple
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let formatted = format (Proxy :: Proxy "Hi %s! You are %d") "Bill" 12
  putStrLn formatted -- "Hi Bill! You are 12"
