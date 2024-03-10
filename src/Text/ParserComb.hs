-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE FunctionalDependencies #-}
module Text.ParserComb(
  Prsr, runPrsr,
  satisfy, satisfyM,
  satisfyMany,
  choice,
  many, emany, optional, eoptional,
  some, esome,
  esepBy, sepBy1, esepBy1,
  esepEndBy, esepEndBy1,
  (<?>), (<|<),
  --notFollowedBy,
  --lookAhead,
  nextToken,
  LastFail(..),
  TokenMachine(..),
  mapTokenState,
  ) where
--Ximport Prelude()
import Prelude
import Control.Applicative
import Control.Monad

data LastFail t
  = LastFail Int [t] [String]
  deriving (Show)

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

data Res tm t a = Many [(a, tm)] (LastFail t)
  --deriving (Show)

data Prsr tm t a = P (tm -> Res tm t a)
--instance Show (Prsr s t a) where show _ = "<<Prsr>>"

runP :: forall tm t a . Prsr tm t a -> (tm -> Res tm t a)
runP (P p) = p

mapTokenState :: forall tm t . (tm -> tm) -> Prsr tm t ()
mapTokenState f = P (\ tm -> Many [((), f tm)] noFail)

instance forall tm t . Functor (Prsr tm t) where
  fmap f p = P $ \ t ->
    case runP p t of
      Many aus lf -> Many [ (f a, u) | (a, u) <- aus ] lf

instance forall tm t . Applicative (Prsr tm t) where
  pure a = P $ \ t -> Many [(a, t)] noFail
  (<*>) = ap
  (*>) p k = p >>= \ _ -> k

instance forall tm t . Monad (Prsr tm t) where
  (>>=) p k = P $ \ t ->
    case runP p t of
      Many aus plf ->
        let ms = map (\ (a, u) -> runP (k a) u) aus
            lfs = map (\ (Many _ lf) -> lf) ms
            rrs = [ r | Many rs _ <- ms, r <- rs ]
        in  Many rrs (longests (plf : lfs))
  return = pure

instance forall t tm . TokenMachine tm t => MonadFail (Prsr tm t) where
  fail m = P $ \ ts -> Many [] (LastFail (tmLeft ts) (firstToken ts) [m])

instance forall t tm . TokenMachine tm t => Alternative (Prsr tm t) where
  empty = P $ \ ts -> Many [] (LastFail (tmLeft ts) (firstToken ts) ["empty"])

  (<|>) p q = P $ \ t ->
    case runP p t of
      Many a lfa ->
        case runP q t of
          Many b lfb -> Many (a ++ b) (longest lfa lfb)

-- Left biased choice
infixl 3 <|<
(<|<) :: forall tm t a . Prsr tm t a -> Prsr tm t a -> Prsr tm t a
(<|<) p q = P $ \ t ->
  case runP p t of
    Many [] lfa ->
      case runP q t of
         Many b lfb -> Many b (longest lfa lfb)
    r -> r

satisfy :: forall tm t . TokenMachine tm t => String -> (t -> Bool) -> Prsr tm t t
satisfy msg f = P $ \ acs ->
  case tmNextToken acs of
    r@(c, _) | f c -> Many [r] noFail
    _ -> Many [] (LastFail (tmLeft acs) (firstToken acs) [msg])

satisfyM :: forall tm t a . TokenMachine tm t => String -> (t -> Maybe a) -> Prsr tm t a
satisfyM msg f = P $ \ acs ->
  case tmNextToken acs of
    (c, cs) | Just a <- f c -> Many [(a, cs)] noFail
    _ -> Many [] (LastFail (tmLeft acs) (firstToken acs) [msg])

satisfyMany :: forall tm t . TokenMachine tm t => (t -> Bool) -> Prsr tm t [t]
satisfyMany f = P $ loop []
  where loop res acs =
          case tmNextToken acs of
            (c, cs) | f c -> loop (c:res) cs
                    | otherwise -> Many [(reverse res, acs)] noFail

infixl 9 <?>
(<?>) :: forall tm t a . Prsr tm t a -> String -> Prsr tm t a
(<?>) p e = P $ \ t ->
--  trace ("<?> " ++ show e) $
  case runP p t of
    Many rs (LastFail l ts _) -> Many rs (LastFail l ts [e])

{-
lookAhead :: forall tm t a . TokenMachine tm t => Prsr tm t a -> Prsr tm t ()
lookAhead p = P $ \ t ->
  case runP p t of
    Many [] (LastFail l ts xs) -> Many [] (LastFail l (take 1 ts) xs)
    _                          -> Many [((), t)] noFail
-}

nextToken :: forall tm t . TokenMachine tm t => Prsr tm t t
nextToken = P $ \ cs ->
  case tmNextToken cs of
    (c, _) -> Many [(c, cs)] noFail

{-
eof :: forall tm t . TokenMachine tm t => Prsr tm t ()
eof = P $ \ t@(cs, _) ->
  case tmNextToken cs of
    Nothing -> Many [((), t)] noFail
    Just _  -> Many [] (LastFail (tmLeft cs) (firstToken cs) ["eof"])
-}

{-
notFollowedBy :: forall t a . Prsr t a -> Prsr t ()
notFollowedBy p = P $ \ t@(ts,_) ->
  case runP p t of
    Many [] _ -> Many [((), t)] noFail
    _         -> Many [] (LastFail (length ts) (take 1 ts) ["!"])
-}

runPrsr :: forall tm t a . --X(Show a, Show s) =>
           Prsr tm t a -> tm -> Either (LastFail t) [a]
runPrsr (P p) f =
  case p f of
    Many [] lf -> Left lf
    Many xs _  -> Right [a | (a, _) <- xs ]

-------------------------------

emany :: forall tm t a . Prsr tm t a -> Prsr tm t [a]
emany p = esome p <|< pure []

esome :: forall tm t a . Prsr tm t a -> Prsr tm t [a]
esome p = (:) <$> p <*> emany p

eoptional :: forall tm t a . Prsr tm t a -> Prsr tm t (Maybe a)
eoptional p = (Just <$> p) <|< pure Nothing

choice :: forall tm t a . TokenMachine tm t => [Prsr tm t a] -> Prsr tm t a
choice [] = empty
choice ps = foldr1 (<|>) ps

sepBy1 :: forall tm t a sep . TokenMachine tm t => Prsr tm t a -> Prsr tm t sep -> Prsr tm t [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

esepBy1 :: forall tm t a sep . Prsr tm t a -> Prsr tm t sep -> Prsr tm t [a]
esepBy1 p sep = (:) <$> p <*> emany (sep *> p)

esepBy :: forall tm t a sep . Prsr tm t a -> Prsr tm t sep -> Prsr tm t [a]
esepBy p sep = esepBy1 p sep <|< pure []

esepEndBy :: forall tm t a sep . Prsr tm t a -> Prsr tm t sep -> Prsr tm t [a]
esepEndBy p sep = esepEndBy1 p sep <|< pure []

esepEndBy1 :: forall tm t a sep . Prsr tm t a -> Prsr tm t sep -> Prsr tm t [a]
esepEndBy1 p sep = (:) <$> p <*> ((sep *> esepEndBy p sep) <|< pure [])

