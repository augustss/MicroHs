-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ReadPrec
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (uses Text.ParserCombinators.ReadP)
--
-- This library defines parser combinators for precedence parsing.

-----------------------------------------------------------------------------

module Text.ParserCombinators.ReadPrec
  (
  ReadPrec,

  -- * Precedences
  Prec,
  minPrec,

  -- * Precedence operations
  lift,
  prec,
  step,
  reset,

  -- * Other operations
  -- | All are based directly on their similarly-named 'ReadP' counterparts.
  get,
  look,
  (+++),
  (<++),
  pfail,
  choice,

  -- * Converters
  readPrec_to_P,
  readP_to_Prec,
  readPrec_to_S,
  readS_to_Prec,
  )
 where


import Text.ParserCombinators.ReadP
  ( ReadP
  , ReadS
  , readP_to_S
  , readS_to_P
  )

import qualified Text.ParserCombinators.ReadP as ReadP
  ( get
  , look
  , (+++), (<++)
  , pfail
  )

import Control.Applicative
import Control.Alternative
import Control.Monad
import Data.Char
import Data.Int
import Data.List
import Data.Num
import Data.Ord

-- ---------------------------------------------------------------------------
-- The readPrec type

newtype ReadPrec a = P (Prec -> ReadP a)

-- Functor, Monad, MonadPlus

-- | @since 2.01
instance Functor ReadPrec where
  fmap h (P f) = P (\n -> fmap h (f n))

-- | @since 4.6.0.0
instance Applicative ReadPrec where
    pure x  = P (\_ -> pure x)
    (<*>) = ap
    liftA2 = liftM2

-- | @since 2.01
instance Monad ReadPrec where
  P f >>= k = P (\n -> do {a <- f n; let {P f' = k a}; f' n})

-- | @since 4.9.0.0
instance MonadFail ReadPrec where
  fail s    = P (\_ -> fail s)

-- | @since 2.01
instance MonadPlus ReadPrec

-- | @since 4.6.0.0
instance Alternative ReadPrec where
  empty = pfail
  (<|>) = (+++)

-- precedences
type Prec = Int

minPrec :: Prec
minPrec = 0

-- ---------------------------------------------------------------------------
-- Operations over ReadPrec

lift :: forall a . ReadP a -> ReadPrec a
-- ^ Lift a precedence-insensitive 'ReadP' to a 'ReadPrec'.
lift m = P (\_ -> m)

step :: forall a . ReadPrec a -> ReadPrec a
-- ^ Increases the precedence context by one.
step (P f) = P (\n -> f (n+1))

reset :: forall a . ReadPrec a -> ReadPrec a
-- ^ Resets the precedence context to zero.
reset (P f) = P (\_ -> f minPrec)

prec :: forall a . Prec -> ReadPrec a -> ReadPrec a
-- ^ @(prec n p)@ checks whether the precedence context is
--   less than or equal to @n@, and
--
--   * if not, fails
--
--   * if so, parses @p@ in context @n@.
prec n (P f) = P (\c -> if c <= n then f n else ReadP.pfail)

-- ---------------------------------------------------------------------------
-- Derived operations

get :: ReadPrec Char
-- ^ Consumes and returns the next character.
--   Fails if there is no input left.
get = lift ReadP.get

look :: ReadPrec String
-- ^ Look-ahead: returns the part of the input that is left, without
--   consuming it.
look = lift ReadP.look

(+++) :: forall a . ReadPrec a -> ReadPrec a -> ReadPrec a
-- ^ Symmetric choice.
P f1 +++ P f2 = P (\n -> f1 n ReadP.+++ f2 n)

(<++) :: forall a . ReadPrec a -> ReadPrec a -> ReadPrec a
-- ^ Local, exclusive, left-biased choice: If left parser
--   locally produces any result at all, then right parser is
--   not used.
P f1 <++ P f2 = P (\n -> f1 n ReadP.<++ f2 n)

pfail :: forall a . ReadPrec a
-- ^ Always fails.
pfail = lift ReadP.pfail

choice :: forall a . [ReadPrec a] -> ReadPrec a
-- ^ Combines all parsers in the specified list.
choice ps = foldr (+++) pfail ps

-- ---------------------------------------------------------------------------
-- Converting between ReadPrec and Read

readPrec_to_P :: forall a . ReadPrec a -> (Int -> ReadP a)
readPrec_to_P (P f) = f

readP_to_Prec :: forall a . (Int -> ReadP a) -> ReadPrec a
readP_to_Prec f = P f

readPrec_to_S :: forall a . ReadPrec a -> (Int -> ReadS a)
readPrec_to_S (P f) n = readP_to_S (f n)

readS_to_Prec :: forall a . (Int -> ReadS a) -> ReadPrec a
readS_to_Prec f = P (\n -> readS_to_P (f n))
