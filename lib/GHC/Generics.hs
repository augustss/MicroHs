-- This is a dummy module so deriving Generic can be spoofed.
module GHC.Generics(
  Generic(..), Rep,
  Generic1(..), Rep1,
  V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..),
  (:+:)(..), (:*:)(..), (:.:)(..),
  ) where

import Control.Applicative(Alternative(..))
import Control.Monad(MonadPlus(..))
import Control.Monad.Fix
import Data.Coerce(coerce)
import Data.Traversable

-----------------
-- Imports for 'deriving instance'
--import Data.Complex
import Data.Int
import Data.Monoid
import Data.Ratio
import Data.Tuple
import Data.Word
-----------------

class Generic a where
  from :: a -> Rep a x
  to :: Rep a x -> a
  from _ = errGeneric
  to _ = errGeneric

type Rep :: Type -> Type -> Type
data Rep a x

type Generic1 :: forall (k :: Kind) . (k -> Type) -> Constraint
class Generic1 f where
  from1 :: f a -> Rep1 f a
  to1 :: Rep1 f a -> f a
  from1 _ = errGeneric
  to1 _ = errGeneric

type Rep1 :: forall (k :: Kind) . (k -> Type) -> k -> Type
data Rep1 f a

errGeneric :: a
errGeneric = error "MicroHs does not implement Generic yet"

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

-- | @since 4.9.0.0
deriving instance Foldable V1

-- | @since 4.9.0.0
deriving instance Traversable V1

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

-- | @since 4.9.0.0
instance Foldable U1 where
    foldMap _ _ = mempty
    fold _ = mempty
    foldr _ z _ = z
    foldl _ z _ = z
    foldl1 _ _ = error "foldl1: U1"
    foldr1 _ _ = error "foldr1: U1"
    length _   = 0
    null _     = True
    elem _ _   = False
    sum _      = 0
    product _  = 1

-- | @since 4.9.0.0
instance Traversable U1 where
    traverse _ _ = pure U1
    sequenceA _ = pure U1
    mapM _ _ = pure U1
    sequence _ = pure U1

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

-- | @since base-4.9.0.0
instance MonadFix Par1 where
    mfix f = Par1 (fix (unPar1 . f))

-- | @since base-4.12.0.0
deriving instance Semigroup p => Semigroup (Par1 p)

-- | @since base-4.12.0.0
deriving instance Monoid p => Monoid (Par1 p)

-- | @since 4.9.0.0
deriving instance Foldable Par1

-- | @since 4.9.0.0
deriving instance Traversable Par1

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

-- | @since base-4.9.0.0
instance MonadFix f => MonadFix (Rec1 f) where
    mfix f = Rec1 (mfix (unRec1 . f))

-- | @since base-4.12.0.0
deriving instance Semigroup (f p) => Semigroup (Rec1 f p)

-- | @since base-4.12.0.0
deriving instance Monoid (f p) => Monoid (Rec1 f p)

-- | @since 4.9.0.0
deriving instance Foldable f => Foldable (Rec1 f)

-- | @since 4.9.0.0
deriving instance Traversable f => Traversable (Rec1 f)

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

-- | @since 4.9.0.0
deriving instance Foldable (K1 i c)

-- | @since 4.9.0.0
deriving instance Traversable (K1 i c)

-- | @since base-4.9.0.0
deriving instance Applicative f => Applicative (M1 i c f)

-- | @since base-4.9.0.0
deriving instance Alternative f => Alternative (M1 i c f)

-- | @since base-4.9.0.0
deriving instance Monad f => Monad (M1 i c f)

-- | @since base-4.9.0.0
deriving instance MonadPlus f => MonadPlus (M1 i c f)

-- | @since base-4.9.0.0
instance MonadFix f => MonadFix (M1 i c f) where
    mfix f = M1 (mfix (unM1 . f))

-- | @since base-4.12.0.0
deriving instance Semigroup (f p) => Semigroup (M1 i c f p)

-- | @since base-4.12.0.0
deriving instance Monoid (f p) => Monoid (M1 i c f p)

-- | @since 4.9.0.0
deriving instance Foldable f => Foldable (M1 i c f)

-- | @since 4.9.0.0
deriving instance Traversable f => Traversable (M1 i c f)

-- | Meta-information (constructor names, etc.)
type M1 :: forall (k :: Kind) . Type -> Type -> (k -> Type) -> k -> Type
-- XXX: c :: Meta
newtype M1 i c f p =
    M1 { unM1 :: f p }
  deriving ( Eq       -- ^ @since base-4.7.0.0
           , Ord      -- ^ @since base-4.7.0.0
           , Read     -- ^ @since base-4.7.0.0
           , Show     -- ^ @since base-4.7.0.0
           , Functor  -- ^ @since base-4.9.0.0
           , Generic  -- ^ @since base-4.7.0.0
           , Generic1 -- ^ @since base-4.9.0.0
           )

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

-- | @since 4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :+: g)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :+: g)

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

-- | @since base-4.9.0.0
instance (MonadFix f, MonadFix g) => MonadFix (f :*: g) where
    mfix f = mfix (fstP . f) :*: mfix (sndP . f)
      where
        fstP (a :*: _) = a
        sndP (_ :*: b) = b

-- | @since base-4.12.0.0
instance (Semigroup (f p), Semigroup (g p)) => Semigroup ((f :*: g) p) where
  (x1 :*: y1) <> (x2 :*: y2) = (x1 <> x2) :*: (y1 <> y2)

-- | @since base-4.12.0.0
instance (Monoid (f p), Monoid (g p)) => Monoid ((f :*: g) p) where
  mempty = mempty :*: mempty

-- | @since 4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :*: g)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :*: g)

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

-- | @since 4.9.0.0
deriving instance (Foldable f, Foldable g) => Foldable (f :.: g)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :.: g)

----------------------------------------------------------------------------

-- deriving Data.Complex causes a cycle.
-- Maybe break up this module to keep the Rep types in another module?

deriving instance Generic Bool
deriving instance Generic Char
deriving instance Generic Int
deriving instance Generic Int8
deriving instance Generic Int16
deriving instance Generic Int32
deriving instance Generic Int64
deriving instance Generic Integer
deriving instance Generic Word
deriving instance Generic Word8
deriving instance Generic Word16
deriving instance Generic Word32
deriving instance Generic Word64

--deriving instance Generic (Complex a)
deriving instance Generic [a]
deriving instance Generic (Maybe a)
deriving instance Generic (Ratio a)

deriving instance Generic (Either a b)

--deriving instance Generic1 Complex
deriving instance Generic1 []
deriving instance Generic1 Maybe
deriving instance Generic1 Ratio

deriving instance Generic ()
deriving instance Generic (Solo a)
deriving instance Generic ((,) a b)
deriving instance Generic ((,,) a b c)
deriving instance Generic ((,,,) a b c d)
deriving instance Generic ((,,,,) a b c d e)
deriving instance Generic ((,,,,,) a b c d e f)
deriving instance Generic ((,,,,,,) a b c d e f g)
deriving instance Generic ((,,,,,,,) a b c d e f g h)
deriving instance Generic ((,,,,,,,,) a b c d e f g h i)
deriving instance Generic ((,,,,,,,,,) a b c d e f g h i j)
deriving instance Generic ((,,,,,,,,,,) a b c d e f g h i j k)
deriving instance Generic ((,,,,,,,,,,,) a b c d e f g h i j k l)
deriving instance Generic ((,,,,,,,,,,,,) a b c d e f g h i j k l m)
deriving instance Generic ((,,,,,,,,,,,,,) a b c d e f g h i j k l m n)
deriving instance Generic ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o)

deriving instance Generic (First a)
deriving instance Generic (Last a)
deriving instance Generic (Ap f a)
deriving instance Generic1 First
deriving instance Generic1 Last
deriving instance Generic1 (Ap f)
