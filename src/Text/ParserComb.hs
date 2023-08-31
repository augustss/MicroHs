-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Text.ParserComb(
  (>>=), (>>), pure,
  (<*), (*>), (<*>), (<$), (<$>),
  (<|>),
  fail, guard,
  get, put, modify,
  Prsr, runPrsr,
  satisfy, satisfyM, eof,
  choice,
  many, emany, optional, eoptional,
  some, esome,
  esepBy, sepBy1, esepBy1,
  (<?>), (<|<),
  notFollowedBy, lookAhead,
  inject, nextToken,
  LastFail(..)
  ) where
--Ximport Prelude()
import PreludeNoIO
--import Debug.Trace
--import Compat

data LastFail t
  = LastFail Int [t] [(String, [String])]
  --Xderiving (Show)

maxInt :: Int
maxInt = 1000000000

noFail :: forall t . LastFail t
noFail = LastFail maxInt [] []

longest :: forall t . LastFail t -> LastFail t -> LastFail t
longest lf1@(LastFail l1 t1 x1) lf2@(LastFail l2 t2 x2) =
  if l1 < l2 then
    lf1
  else if l2 < l1 then
    lf2
  else
    LastFail l1 (t1 ++ t2) (x1 ++ x2)

longests :: forall t . [LastFail t] -> LastFail t
longests xs = foldl1 longest xs

data Res s t a = Many [(a, ([t], s))] (LastFail t)
  --deriving (Show)

data Prsr s t a = P (([t], s) -> Res s t a)
--instance Show (Prsr s t a) where show _ = "<<Prsr>>"

runP :: forall s t a . Prsr s t a -> (([t], s) -> Res s t a)
runP (P p) = p

pure :: forall s t a . a -> Prsr s t a
pure a = P $ \ t -> Many [(a, t)] noFail

--Xinfixl 1 >>=
--Yinfixl 1 >>=
(>>=) :: forall s t a b . Prsr s t a -> (a -> Prsr s t b) -> Prsr s t b
(>>=) p k = P $ \ t ->
    case runP p t of
      Many aus plf ->
        let { xss = [ runP (k a) u | au <- aus, let { (a, u) = au } ] }
        in  case unzip [ (rs, lf) | xs <- xss, let { Many rs lf = xs } ] of
              (rss, lfs) -> Many (concat rss) (longests (plf : lfs))

-- XXX needs (x,y) <- e

--Xinfixl 1 >>
--Yinfixl 1 >>
(>>) :: forall s t a b . Prsr s t a -> Prsr s t b -> Prsr s t b
(>>) p k = p >>= \ _ -> k

--Xinfixl 4 <*>
--Yinfixl 4 <*>
(<*>) :: forall s t a b . Prsr s t (a -> b) -> Prsr s t a -> Prsr s t b
(<*>) m1 m2 = m1 >>= \ x1 -> m2 >>= \ x2 -> pure (x1 x2)

--Xinfixl 4 <*
--Yinfixl 4 <*
(<*) :: forall s t a b . Prsr s t a -> Prsr s t b -> Prsr s t a
(<*) m1 m2 = m1 >>= \ x1 -> m2 >> pure x1

--Xinfixl 4 *>
--Yinfixl 4 *>
(*>) :: forall s t a b . Prsr s t a -> Prsr s t b -> Prsr s t b
(*>) m1 m2 = m1 >> m2 >>= \ x2 -> pure x2

--Xinfixl 4 <$>
--Yinfixl 4 <$>
(<$>) :: forall s t a b . (a -> b) -> Prsr s t a -> Prsr s t b
(<$>) f p = P $ \ t ->
    case runP p t of
      Many aus lf -> Many [ (f a, u) | (a, u) <- aus ] lf

--Xinfixl 4 <$
--Yinfixl 4 <$
(<$) :: forall s t a b . a -> Prsr s t b -> Prsr s t a
(<$) a p = p >> pure a

guard :: forall s t . Bool -> Prsr s t ()
guard b = if b then pure () else empty

empty :: forall s t a . Prsr s t a
empty = P $ \ (ts, _) -> Many [] (LastFail (length ts) (take 1 ts) [("empty", ["<empty>"])])

--Xinfixl 3 <|>
--Yinfixl 3 <|>
(<|>) :: forall s t a . Prsr s t a -> Prsr s t a -> Prsr s t a
(<|>) p q = P $ \ t ->
    case runP p t of
      Many a lfa ->
        case runP q t of
          Many b lfb -> Many (a ++ b) (longest lfa lfb)

fail :: forall s t a . String -> Prsr s t a
fail m = P $ \ (ts, _) -> Many [] (LastFail (length ts) (take 1 ts) [("fail", [m])])

get :: forall s t . Prsr s t s
get = P $ \ t@(_, s) -> Many [(s, t)] noFail

put :: forall s t . s -> Prsr s t ()
put s = P $ \ (ts, _) -> Many [((), (ts, s))] noFail

modify :: forall s t . (s -> s) -> Prsr s t ()
modify f = get >>= \ s -> put (f s)

-- Left biased choice
(<|<) :: forall s t a . Prsr s t a -> Prsr s t a -> Prsr s t a
(<|<) p q = P $ \ t ->
  case runP p t of
    Many [] lfa ->
      case runP q t of
         Many b lfb -> Many b (longest lfa lfb)
    r -> r

many :: forall s t a . Prsr s t a -> Prsr s t [a]
many p = some p <|> pure []

some :: forall s t a . Prsr s t a -> Prsr s t [a]
some p = (:) <$> p <*> many p

optional :: forall s t a . Prsr s t a -> Prsr s t (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

emany :: forall s t a . Prsr s t a -> Prsr s t [a]
emany p = esome p <|< pure []

esome :: forall s t a . Prsr s t a -> Prsr s t [a]
esome p = (:) <$> p <*> emany p

eoptional :: forall s t a . Prsr s t a -> Prsr s t (Maybe a)
eoptional p = (Just <$> p) <|< pure Nothing

runPrsr :: forall s t a . --X(Show a, Show s) =>
           s -> Prsr s t a -> [t] -> Either (LastFail t) [(a, s)]
runPrsr s (P p) f =
  case p (f, s) of
    Many [] lf -> Left lf
    Many xs _  -> Right [(a, snd x) | (a, x) <- xs ]

choice :: forall s t a . [Prsr s t a] -> Prsr s t a
choice [] = empty
choice ps = foldr1 (<|>) ps

satisfy :: forall s t . String -> (t -> Bool) -> Prsr s t t
satisfy msg f = P $ \ (acs, s) ->
  case acs of
    c:cs | f c -> Many [(c, (cs, s))] noFail
    _ -> Many [] (LastFail (length acs) (take 1 acs) [("satisfy", [msg])])

satisfyM :: forall s t a . String -> (t -> Maybe a) -> Prsr s t a
satisfyM msg f = P $ \ (acs, s) ->
  case acs of
    c:cs | Just a <- f c -> Many [(a, (cs, s))] noFail
    _ -> Many [] (LastFail (length acs) (take 1 acs) [("satisfyM", [msg])])

eof :: forall s t . Prsr s t ()
eof = P $ \ t@(cs, _) ->
 if null cs then
   Many [((), t)] noFail
 else
   Many [] (LastFail (length cs) (take 1 cs) [("eof", ["end-of-file"])])

(<?>) :: forall s t a . Prsr s t a -> String -> Prsr s t a
(<?>) p e = P $ \ t ->
--  trace ("<?> " ++ show e) $
  case runP p t of
    Many rs (LastFail l ts xs) ->
      Many rs (LastFail l ts [(m, e:es) | (m, es) <- xs ])

notFollowedBy :: forall s t a . Prsr s t a -> Prsr s t ()
notFollowedBy p = P $ \ t@(ts,_) ->
  case runP p t of
    Many [] _ -> Many [((), t)] noFail
    _         -> Many [] (LastFail (length ts) (take 1 ts) [("notFollowedBy", [])])

lookAhead :: forall s t a . Prsr s t a -> Prsr s t ()
lookAhead p = P $ \ t ->
  case runP p t of
    Many [] (LastFail l ts xs) -> Many [] (LastFail l (take 1 ts) [("lookAhead-" ++ m, es) | (m, es) <- xs ])
    _                          -> Many [((), t)] noFail

nextToken :: forall s t . Prsr s t t
nextToken = P $ \ t@(cs, _) ->
  case cs of
    [] ->  Many [] (LastFail (length cs) [] [("nextToken", [])])
    c:_ -> Many [(c, t)] noFail

inject :: forall s t . [t] -> Prsr s t ()
inject s = P $ \ (cs, st) -> Many [((), (s ++ cs, st))] noFail

sepBy1 :: forall s t a sep . Prsr s t a -> Prsr s t sep -> Prsr s t [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

esepBy1 :: forall s t a sep . Prsr s t a -> Prsr s t sep -> Prsr s t [a]
esepBy1 p sep = (:) <$> p <*> emany (sep *> p)

esepBy :: forall s t a sep . Prsr s t a -> Prsr s t sep -> Prsr s t [a]
esepBy p sep = esepBy1 p sep <|< pure []
