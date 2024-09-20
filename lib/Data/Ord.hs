module Data.Ord(
  module Data.Ord,
  module Data.Ordering_Type,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Bool_Type
import Data.Bounded
import Data.Ordering_Type
import Data.Eq
import Text.Show

infix 4 <,<=,>,>=

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

  compare x y = if x == y then EQ
                else if x <= y then LT
                else GT

  x <= y = case compare x y of { GT -> False; _ -> True }
  x >= y = y <= x
  x > y = if x <= y then False else True
  x < y = if y <= x then False else True

  min x y = if x <= y then x else y
  max x y = if x <= y then y else x

instance Eq Ordering where
  LT == LT  =  True
  EQ == EQ  =  True
  GT == GT  =  True
  _  == _   =  False

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance Bounded Ordering where
  minBound = LT
  maxBound = GT

comparing :: (Ord b) => (a -> b) -> a -> a -> Ordering
comparing f x y = compare (f x) (f y)

{-
newtype Down a = Down
    { getDown :: a -- ^ @since 4.14.0.0
    }
    deriving
      ( Eq        -- ^ @since 4.6.0.0
      , Num       -- ^ @since 4.11.0.0
      , Semigroup -- ^ @since 4.11.0.0
      , Monoid    -- ^ @since 4.11.0.0
      , Bits       -- ^ @since 4.14.0.0
      , FiniteBits -- ^ @since 4.14.0.0
      , Floating   -- ^ @since 4.14.0.0
      , Fractional -- ^ @since 4.14.0.0
      , Ix         -- ^ @since 4.14.0.0
      , Real       -- ^ @since 4.14.0.0
      , RealFrac   -- ^ @since 4.14.0.0
      , RealFloat  -- ^ @since 4.14.0.0
      , Storable   -- ^ @since 4.14.0.0
      )

-- | This instance would be equivalent to the derived instances of the
-- 'Down' newtype if the 'getDown' field were removed
--
-- @since 4.7.0.0
instance (Read a) => Read (Down a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Down x,t) | ("Down",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Down' newtype if the 'getDown' field were removed
--
-- @since 4.7.0.0
instance (Show a) => Show (Down a) where
    showsPrec d (Down x) = showParen (d > 10) $
        showString "Down " . showsPrec 11 x

-- | @since 4.6.0.0
instance Ord a => Ord (Down a) where
    compare (Down x) (Down y) = y `compare` x

-- | Swaps @'minBound'@ and @'maxBound'@ of the underlying type.
--
-- @since 4.14.0.0
instance Bounded a => Bounded (Down a) where
    minBound = Down maxBound
    maxBound = Down minBound

-- | @since 4.11.0.0
instance Functor Down where
    fmap = coerce

-- | @since 4.11.0.0
instance Applicative Down where
    pure = Down
    (<*>) = coerce

-- | @since 4.11.0.0
instance Monad Down where
    Down a >>= k = k a
-}
