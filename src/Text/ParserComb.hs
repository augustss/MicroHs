-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE FunctionalDependencies #-}
module Text.ParserComb(
  get, put, modify,
  Prsr, runPrsr,
  satisfy, satisfyM,
  choice,
  many, emany, optional, eoptional,
  some, esome,
  esepBy, sepBy1, esepBy1,
  esepEndBy, esepEndBy1,
  (<?>), (<|<),
  --notFollowedBy,
  lookAhead,
  nextToken,
  LastFail(..),
  TokenMachine(..),
  mapTokenState,
  ) where
--Ximport Prelude()
import Prelude
import Control.Applicative
import Control.Monad
import Compat

data LastFail t
  = LastFail Int [t] [String]
  --Xderiving (Show)

maxInt :: Int
maxInt = 1000000000

noFail :: forall t . LastFail t
noFail = LastFail maxInt [] []

longest :: forall t . LastFail t -> LastFail t -> LastFail t
longest lf1@(LastFail l1 t1 x1) lf2@(LastFail l2 _ x2) =
  if l1 < l2 then
    lf1
  else if l2 < l1 then
    lf2
  else
    LastFail l1 t1 (x1 ++ x2)

longests :: forall t . [LastFail t] -> LastFail t
longests xs = foldl1 longest xs

class TokenMachine tm t | tm -> t where
  tmNextToken :: tm -> (t, tm)
  tmRawTokens :: tm -> [t]

tmLeft :: forall tm t . TokenMachine tm t => tm -> Int
tmLeft = length . tmRawTokens

firstToken :: forall tm t . TokenMachine tm t => tm -> [t]
firstToken tm =
  case tmNextToken tm of
    (t, _) -> [t]

data Res s tm t a = Many [(a, (tm, s))] (LastFail t)
  --deriving (Show)

data Prsr s tm t a = P ((tm, s) -> Res s tm t a)
--instance Show (Prsr s t a) where show _ = "<<Prsr>>"

runP :: forall s tm t a . Prsr s tm t a -> ((tm, s) -> Res s tm t a)
runP (P p) = p

mapTokenState :: forall s tm t . (tm -> tm) -> Prsr s tm t ()
mapTokenState f = P (\ (tm, s) -> Many [((), (f tm, s))] noFail)

instance forall s tm t . Functor (Prsr s tm t) where
  fmap f p = P $ \ t ->
    case runP p t of
      Many aus lf -> Many [ (f a, u) | (a, u) <- aus ] lf

instance forall s tm t . Applicative (Prsr s tm t) where
  pure a = P $ \ t -> Many [(a, t)] noFail
  (<*>) = ap
  (*>) p k = p >>= \ _ -> k

instance forall s tm t . Monad (Prsr s tm t) where
  (>>=) p k = P $ \ t ->
    case runP p t of
      Many aus plf ->
        let ms = map (\ (a, u) -> runP (k a) u) aus
            lfs = map (\ (Many _ lf) -> lf) ms
            rrs = [ r | Many rs _ <- ms, r <- rs ]
        in  Many rrs (longests (plf : lfs))
  return = pure

instance forall s t tm . TokenMachine tm t => MonadFail (Prsr s tm t) where
  fail m = P $ \ (ts, _) -> Many [] (LastFail (tmLeft ts) (firstToken ts) [m])

instance forall s t tm . TokenMachine tm t => Alternative (Prsr s tm t) where
  empty = P $ \ (ts, _) -> Many [] (LastFail (tmLeft ts) (firstToken ts) [])

  (<|>) p q = P $ \ t ->
    case runP p t of
      Many a lfa ->
        case runP q t of
          Many b lfb -> Many (a ++ b) (longest lfa lfb)

get :: forall s tm t . Prsr s tm t s
get = P $ \ t@(_, s) -> Many [(s, t)] noFail

put :: forall s tm t . s -> Prsr s tm t ()
put s = P $ \ (ts, _) -> Many [((), (ts, s))] noFail

modify :: forall s tm t . (s -> s) -> Prsr s tm t ()
modify f = get >>= \ s -> put (f s)

-- Left biased choice
infixl 3 <|<
(<|<) :: forall s tm t a . Prsr s tm t a -> Prsr s tm t a -> Prsr s tm t a
(<|<) p q = P $ \ t ->
  case runP p t of
    Many [] lfa ->
      case runP q t of
         Many b lfb -> Many b (longest lfa lfb)
    r -> r

satisfy :: forall s tm t . TokenMachine tm t => String -> (t -> Bool) -> Prsr s tm t t
satisfy msg f = P $ \ (acs, s) ->
  case tmNextToken acs of
    (c, cs) | f c -> Many [(c, (cs, s))] noFail
    _ -> Many [] (LastFail (tmLeft acs) (firstToken acs) [msg])

satisfyM :: forall s tm t a . TokenMachine tm t => String -> (t -> Maybe a) -> Prsr s tm t a
satisfyM msg f = P $ \ (acs, s) ->
  case tmNextToken acs of
    (c, cs) | Just a <- f c -> Many [(a, (cs, s))] noFail
    _ -> Many [] (LastFail (tmLeft acs) (firstToken acs) [msg])

infixl 9 <?>
(<?>) :: forall s tm t a . Prsr s tm t a -> String -> Prsr s tm t a
(<?>) p e = P $ \ t ->
--  trace ("<?> " ++ show e) $
  case runP p t of
    Many rs (LastFail l ts _) -> Many rs (LastFail l ts [e])

lookAhead :: forall s tm t a . TokenMachine tm t => Prsr s tm t a -> Prsr s tm t ()
lookAhead p = P $ \ t ->
  case runP p t of
    Many [] (LastFail l ts xs) -> Many [] (LastFail l (take 1 ts) xs)
    _                          -> Many [((), t)] noFail

nextToken :: forall s tm t . TokenMachine tm t => Prsr s tm t t
nextToken = P $ \ t@(cs, _) ->
  case tmNextToken cs of
    (c, _) -> Many [(c, t)] noFail

{-
eof :: forall s tm t . TokenMachine tm t => Prsr s tm t ()
eof = P $ \ t@(cs, _) ->
  case tmNextToken cs of
    Nothing -> Many [((), t)] noFail
    Just _  -> Many [] (LastFail (tmLeft cs) (firstToken cs) ["eof"])
-}

{-
notFollowedBy :: forall s t a . Prsr s t a -> Prsr s t ()
notFollowedBy p = P $ \ t@(ts,_) ->
  case runP p t of
    Many [] _ -> Many [((), t)] noFail
    _         -> Many [] (LastFail (length ts) (take 1 ts) ["!"])
-}

runPrsr :: forall s tm t a . --X(Show a, Show s) =>
           s -> Prsr s tm t a -> tm -> Either (LastFail t) [(a, s)]
runPrsr s (P p) f =
  case p (f, s) of
    Many [] lf -> Left lf
    Many xs _  -> Right [(a, snd x) | (a, x) <- xs ]

-------------------------------

emany :: forall s tm t a . Prsr s tm t a -> Prsr s tm t [a]
emany p = esome p <|< pure []

esome :: forall s tm t a . Prsr s tm t a -> Prsr s tm t [a]
esome p = (:) <$> p <*> emany p

eoptional :: forall s tm t a . Prsr s tm t a -> Prsr s tm t (Maybe a)
eoptional p = (Just <$> p) <|< pure Nothing

choice :: forall s tm t a . TokenMachine tm t => [Prsr s tm t a] -> Prsr s tm t a
choice [] = empty
choice ps = foldr1 (<|>) ps

sepBy1 :: forall s tm t a sep . TokenMachine tm t => Prsr s tm t a -> Prsr s tm t sep -> Prsr s tm t [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

esepBy1 :: forall s tm t a sep . Prsr s tm t a -> Prsr s tm t sep -> Prsr s tm t [a]
esepBy1 p sep = (:) <$> p <*> emany (sep *> p)

esepBy :: forall s tm t a sep . Prsr s tm t a -> Prsr s tm t sep -> Prsr s tm t [a]
esepBy p sep = esepBy1 p sep <|< pure []

esepEndBy :: forall s tm t a sep . Prsr s tm t a -> Prsr s tm t sep -> Prsr s tm t [a]
esepEndBy p sep = esepEndBy1 p sep <|< pure []

esepEndBy1 :: forall s tm t a sep . Prsr s tm t a -> Prsr s tm t sep -> Prsr s tm t [a]
esepEndBy1 p sep = (:) <$> p <*> ((sep *> esepEndBy p sep) <|< pure [])

