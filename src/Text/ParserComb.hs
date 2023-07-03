{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.ParserComb(
  module Text.ParserComb
{-
  get, gets, put, modify,
  Prsr, runPrsr,
  satisfy, char, string, eof,
  choice,
  many, emany, optional, eoptional,
  some, esome,
  (<?>), (<|<),
  notFollowedBy, lookAhead,
  inject,
-}
  )where
import Prelude --X hiding (Monad(..), MonadFail(..), Applicative(..), Functor(..), (<$>), showString)
{-
import Control.Monad
import Control.Monad.State.Strict
import Control.Applicative --hiding (many, some)
-}
--import Data.Char
import Data.List
--import Debug.Trace

data LastFail
  = LastFail Int [(String, [String])]
  --deriving (Show)

--XshowString :: String -> String
--XshowString = show
--XshowInt :: Int -> String
--XshowInt = show

maxInt :: Int
maxInt = 1000000000

noFail :: LastFail
noFail = LastFail maxInt []

longest :: LastFail -> LastFail -> LastFail
longest lf1 lf2 =
  case lf1 of
    LastFail l1 x1 ->
      case lf2 of
        LastFail l2 x2 ->
          if l1 < l2 then
            lf1
          else if l2 < l1 then
            lf2
          else
            LastFail l1 (x1 ++ x2)

longests :: [LastFail] -> LastFail
longests xs =
  case xs of
    [] -> undefined
    _:_ -> foldl1 longest xs

data Res s a = Many [(a, (String, s))] LastFail
  --deriving (Show)

data Prsr s a = P ((String, s) -> Res s a)
runP :: Prsr s a -> ((String, s) -> Res s a)
runP pp =
  case pp of
    P p -> p
--instance Show (Prsr s a) where show _ = "<<Prsr>>"

pure :: a -> Prsr s a
pure a = P $ \ t -> Many [(a, t)] noFail

--Xinfixl 1 >>=
(>>=) :: Prsr s a -> (a -> Prsr s b) -> Prsr s b
(>>=) p k = P $ \ t ->
    case runP p t of
      Many aus plf ->
        let { xss = [ runP (k a) u | au <- aus, let { (a, u) = au } ] }
        in  case unzip [ (rs, lf) | xs <- xss, let { Many rs lf = xs } ] of
              (rss, lfs) -> Many (concat rss) (longests $ plf : lfs)

-- XXX needs (x,y) <- e

--Xinfixl 1 >>
(>>) :: Prsr s a -> Prsr s b -> Prsr s b
(>>) p k = p >>= \ _ -> k

--Xinfixl 4 <*>
(<*>) :: Prsr s (a -> b) -> Prsr s a -> Prsr s b
(<*>) m1 m2 = m1 >>= \ x1 -> m2 >>= \ x2 -> pure (x1 x2)

--Xinfixl 4 <*
(<*) :: Prsr s a -> Prsr s b -> Prsr s a
(<*) m1 m2 = m1 >>= \ x1 -> m2 >> pure x1

--Xinfixl 4 *>
(*>) :: Prsr s a -> Prsr s b -> Prsr s b
(*>) m1 m2 = m1 >> m2 >>= \ x2 -> pure x2

--Xinfixl 4 <$>
(<$>) :: (a -> b) -> Prsr s a -> Prsr s b
(<$>) f p = P $ \ t ->
    case runP p t of
      Many aus lf -> Many [ (f a, u) | au <- aus, let { (a, u) = au } ] lf

--Xinfixl 4 <$
(<$) :: a -> Prsr s b -> Prsr s a
(<$) a p = p >> pure a

guard :: Bool -> Prsr s ()
guard b = if b then pure () else empty

empty :: Prsr s a
empty = P $ \ t -> Many [] (LastFail (length (fst t)) [("empty", ["<empty>"])])

--Xinfixl 3 <|>
(<|>) :: Prsr s a -> Prsr s a -> Prsr s a
(<|>) p q = P $ \ t ->
    case runP p t of
      Many a lfa ->
        case runP q t of
          Many b lfb -> Many (a ++ b) (longest lfa lfb)

fail :: String -> Prsr s a
fail m = P $ \ t -> Many [] (LastFail (length (fst t)) [("fail", [m])])

get :: Prsr s s
get = P $ \ t -> Many [(snd t, t)] noFail

put :: s -> Prsr s ()
put s = P $ \ t -> Many [((), (fst t, s))] noFail

modify :: (s -> s) -> Prsr s ()
modify f = get >>= \ s -> put (f s)

-- Left biased choice
(<|<) :: Prsr s a -> Prsr s a -> Prsr s a
(<|<) p q = P $ \ t ->
  let
    r = runP p t
  in case r of
       Many rs lfa ->
         case rs of
           [] ->
             case runP q t of
               Many b lfb -> Many b (longest lfa lfb)
           _:_ -> r

many :: Prsr s a -> Prsr s [a]
many p = some p <|> pure []

some :: Prsr s a -> Prsr s [a]
some p = (:) <$> p <*> many p

optional :: Prsr s a -> Prsr s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

emany :: Prsr s a -> Prsr s [a]
emany p = esome p <|< pure []

esome :: Prsr s a -> Prsr s [a]
esome p = (:) <$> p <*> emany p

eoptional :: Prsr s a -> Prsr s (Maybe a)
eoptional p = (Just <$> p) <|< pure Nothing

--XrunPrsr :: (Show a, Show s) => s -> Prsr s a -> FilePath -> String -> Either String [(a, s)]
runPrsr s pp fn f =
  case pp of
    P p ->
      case p (f, s) of
        Many xs lf ->
          case xs of
            []  -> Left $ formatFailed fn f lf
            _:_ -> Right [(a, snd x) | ax <- xs, let { (a, x) = ax } ]

formatFailed :: FilePath -> String -> LastFail -> String
formatFailed fn file lf =
  case lf of
    LastFail len _ ->
      let
        (pre, post) = splitAt (length file - len) file
        count lc x =
          case lc of
            (l, c) ->
              if x == '\n' then (l+1, 0) else (l, c+1)
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

choice :: [Prsr s a] -> Prsr s a
choice ps =
  case ps of
    [] -> empty
    _:_ -> foldr1 (<|>) ps

satisfy :: String -> (Char -> Bool) -> Prsr s Char
satisfy msg f = P $ \ t ->
  case t of
    (acs, s) ->
      let
        bad = Many [] (LastFail (length acs) [("satisfy", [msg])])
      in
        case acs of
          [] -> bad
          c:cs ->
            if f c then
              Many [(c, (cs, s))] noFail
            else
              bad

char :: Char -> Prsr s Char
char c = satisfy "char" (== c)

string :: String -> Prsr s String
string str = P $ \ t ->
  case t of
    (cs, s) ->
      case stripPrefix str cs of
        Just cs' -> Many [(str, (cs', s))] noFail
        Nothing  -> Many [] (LastFail (length cs) [("string", [showString str])])

eof :: Prsr s ()
eof = P $ \ t ->
  let
    cs = fst t
  in
     if null cs then
       Many [((), t)] noFail
     else
       Many [] (LastFail (length cs) [("eof", ["end-of-file"])])

(<?>) :: Prsr s a -> String -> Prsr s a
(<?>) p e = P $ \ t ->
--  trace ("<?> " ++ show e) $
  case runP p t of
    Many rs lf ->
      case lf of
        LastFail l xs -> Many rs (LastFail l [(m, e:es) | mes <- xs, let { (m, es) = mes } ])

notFollowedBy :: Prsr s a -> Prsr s ()
notFollowedBy p = P $ \ t ->
  case runP p t of
    Many rs _ ->
      if null rs then
        Many [((), t)] noFail
      else
        Many [] (LastFail (length (fst t)) [("notFollowedBy", [])])

lookAhead :: Prsr s a -> Prsr s ()
lookAhead p = P $ \ t ->
  case runP p t of
    Many rs lf ->
      if null rs then
        case lf of
          LastFail l xs -> Many [] (LastFail l [("lookAhead-" ++ m, es) | mes <- xs, let { (m, es) = mes }])
      else
        Many [((), t)] noFail

inject :: String -> Prsr s ()
inject s = P $ \ csst -> case csst of { (cs, st) -> Many [((), (s ++ cs, st))] noFail }
