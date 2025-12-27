-- This is a dummy module so deriving Generic can be spoofed.
module GHC.Generics(
  Generic, Generic1,
  V1, U1(..), Par1(..), Rec1(..), K1(..),
  (:+:)(..), (:*:)(..), (:.:)(..),
  ) where

import Control.Applicative(Alternative(..))
import Control.Monad(MonadPlus(..))
import Data.Coerce(coerce)

class Generic a
class Generic1 f

--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
type V1 :: forall (k :: Kind) . k -> Type
data V1 p
  deriving ( Eq       -- ^ @since base-4.9.0.0
           , Ord      -- ^ @since base-4.9.0.0
           , Read     -- ^ @since base-4.9.0.0
           , Show     -- ^ @since base-4.9.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.9.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | @since base-4.12.0.0
instance Semigroup (V1 p) where
  v <> _ = v

-- | Unit: used for constructors without arguments
type U1 :: forall (k :: Kind) . k -> Type
data U1 p = U1
  deriving ( Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | @since base-4.9.0.0
instance Eq (U1 p) where
  _ == _ = True

-- | @since base-4.7.0.0
instance Ord (U1 p) where
  compare _ _ = EQ

-- | @since base-4.9.0.0
deriving instance Read (U1 p)

-- | @since base-4.9.0.0
instance Show (U1 p) where
  showsPrec _ _ = showString "U1"

-- | @since base-4.9.0.0
instance Functor U1 where
  fmap _ _ = U1

-- | @since base-4.9.0.0
instance Applicative U1 where
  pure _ = U1
  _ <*> _ = U1
  liftA2 _ _ _ = U1

-- | @since base-4.9.0.0
instance Alternative U1 where
  empty = U1
  _ <|> _ = U1

-- | @since base-4.9.0.0
instance Monad U1 where
  _ >>= _ = U1

-- | @since base-4.9.0.0
instance MonadPlus U1

-- | @since base-4.12.0.0
instance Semigroup (U1 p) where
  _ <> _ = U1

-- | @since base-4.12.0.0
instance Monoid (U1 p) where
  mempty = U1

-- | Used for marking occurrences of the parameter
newtype Par1 p = Par1 { unPar1 :: p }
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | @since base-4.9.0.0
instance Applicative Par1 where
  pure = Par1
  (<*>) = coerce
  liftA2 = coerce

-- | @since base-4.9.0.0
instance Monad Par1 where
  Par1 x >>= f = f x

-- | @since base-4.12.0.0
deriving instance Semigroup p => Semigroup (Par1 p)

-- | @since base-4.12.0.0
deriving instance Monoid p => Monoid (Par1 p)

-- | Recursive calls of kind @* -> *@ (or kind @k -> *@, when @PolyKinds@
-- is enabled)
type Rec1 :: forall (k :: Kind) . (k -> Type) -> k -> Type
newtype Rec1 f p = Rec1 { unRec1 :: f p }
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | @since base-4.9.0.0
deriving instance Applicative f => Applicative (Rec1 f)

-- | @since base-4.9.0.0
deriving instance Alternative f => Alternative (Rec1 f)

-- | @since base-4.9.0.0
instance Monad f => Monad (Rec1 f) where
  Rec1 x >>= f = Rec1 (x >>= \a -> unRec1 (f a))

-- | @since base-4.9.0.0
deriving instance MonadPlus f => MonadPlus (Rec1 f)

-- | @since base-4.12.0.0
deriving instance Semigroup (f p) => Semigroup (Rec1 f p)

-- | @since base-4.12.0.0
deriving instance Monoid (f p) => Monoid (Rec1 f p)

-- | Constants, additional parameters and recursion of kind @*@
type K1 :: forall (k :: Kind) . Type -> Type -> k -> Type
newtype K1 i c p = K1 { unK1 :: c }
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | @since base-4.12.0.0
instance Monoid c => Applicative (K1 i c) where
  pure _ = K1 mempty
  liftA2 _ = coerce (mappend :: c -> c -> c)
  (<*>) = coerce (mappend :: c -> c -> c)

-- | @since base-4.12.0.0
deriving instance Semigroup c => Semigroup (K1 i c p)

-- | @since base-4.12.0.0
deriving instance Monoid c => Monoid (K1 i c p)

{-
-- | @since base-4.9.0.0
deriving instance Applicative f => Applicative (M1 i c f)

-- | @since base-4.9.0.0
deriving instance Alternative f => Alternative (M1 i c f)

-- | @since base-4.9.0.0
deriving instance Monad f => Monad (M1 i c f)

-- | @since base-4.9.0.0
deriving instance MonadPlus f => MonadPlus (M1 i c f)

-- | @since base-4.12.0.0
deriving instance Semigroup (f p) => Semigroup (M1 i c f p)

-- | @since base-4.12.0.0
deriving instance Monoid (f p) => Monoid (M1 i c f p)

-- | Meta-information (constructor names, etc.)
newtype M1 (i :: Type) (c :: Meta) (f :: k -> Type) (p :: k) =
    M1 { unM1 :: f p }
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )
-}

-- | Sums: encode choice between constructors
infixr 5 :+:
type (:+:) :: forall (k :: Kind) . (k -> Type) -> (k -> Type) -> k -> Type
data (:+:) f g p = L1 (f p) | R1 (g p)
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
type (:*:) :: forall (k :: Kind) . (k -> Type) -> (k -> Type) -> k -> Type
data (:*:) f g p = f p :*: g p
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | @since base-4.9.0.0
instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a = pure a :*: pure a
  (f :*: g) <*> (x :*: y) = (f <*> x) :*: (g <*> y)
  liftA2 f (a :*: b) (x :*: y) = liftA2 f a x :*: liftA2 f b y

-- | @since base-4.9.0.0
instance (Alternative f, Alternative g) => Alternative (f :*: g) where
  empty = empty :*: empty
  (x1 :*: y1) <|> (x2 :*: y2) = (x1 <|> x2) :*: (y1 <|> y2)

-- | @since base-4.9.0.0
instance (Monad f, Monad g) => Monad (f :*: g) where
  (m :*: n) >>= f = (m >>= \a -> fstP (f a)) :*: (n >>= \a -> sndP (f a))
    where
      fstP (a :*: _) = a
      sndP (_ :*: b) = b

-- | @since base-4.9.0.0
instance (MonadPlus f, MonadPlus g) => MonadPlus (f :*: g)

-- | @since base-4.12.0.0
instance (Semigroup (f p), Semigroup (g p)) => Semigroup ((f :*: g) p) where
  (x1 :*: y1) <> (x2 :*: y2) = (x1 <> x2) :*: (y1 <> y2)

-- | @since base-4.12.0.0
instance (Monoid (f p), Monoid (g p)) => Monoid ((f :*: g) p) where
  mempty = mempty :*: mempty

-- | Composition of functors
infixr 7 :.:
type (:.:) :: forall (k2 :: Kind) (k1 :: Kind) . (k2 -> Type) -> (k1 -> k2) -> k1 -> Type
newtype (:.:) f g p =
    Comp1 { unComp1 :: f (g p) }
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

-- | @since base-4.9.0.0
instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x = Comp1 (pure (pure x))
  Comp1 f <*> Comp1 x = Comp1 (liftA2 (<*>) f x)
  liftA2 f (Comp1 x) (Comp1 y) = Comp1 (liftA2 (liftA2 f) x y)

-- | @since base-4.9.0.0
instance (Alternative f, Applicative g) => Alternative (f :.: g) where
  empty = Comp1 empty
  (<|>) = coerce ((<|>) :: f (g a) -> f (g a) -> f (g a)) ::
    forall a . (f :.: g) a -> (f :.: g) a -> (f :.: g) a

-- | @since base-4.12.0.0
deriving instance Semigroup (f (g p)) => Semigroup ((f :.: g) p)

-- | @since base-4.12.0.0
deriving instance Monoid (f (g p)) => Monoid ((f :.: g) p)
