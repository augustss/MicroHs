module Data.Hashable.Class(
  Hashable(..),
  Hashable1(..),
  Hashable2(..),
  hashWithSalt1,
  hashWithSalt2,
  defaultLiftHashWithSalt,
  ) where
import Data.Functor.Classes
import Data.Functor.Const
import Data.Hashable
import Data.List
import qualified Data.List.NonEmpty as NE

class (Eq1 t {-, forall a. Hashable a => Hashable (t a)-}) => Hashable1 t where
    liftHashWithSalt :: (Int -> a -> Int) -> Int -> t a -> Int

class (Eq2 t {-, forall a. Hashable a => Hashable1 (t a)-}) => Hashable2 t where
    liftHashWithSalt2 :: (Int -> a -> Int) -> (Int -> b -> Int) -> Int -> t a b -> Int

hashWithSalt1 :: (Hashable1 f, Hashable a) => Int -> f a -> Int
hashWithSalt1 = liftHashWithSalt hashWithSalt

hashWithSalt2 :: (Hashable2 f, Hashable a, Hashable b) => Int -> f a b -> Int
hashWithSalt2 = liftHashWithSalt2 hashWithSalt hashWithSalt

defaultLiftHashWithSalt :: (Hashable2 f, Hashable a) => (Int -> b -> Int) -> Int -> f a b -> Int
defaultLiftHashWithSalt h = liftHashWithSalt2 hashWithSalt h

hashUsing :: (Hashable b) =>
             (a -> b)           -- ^ Transformation function.
          -> Int                -- ^ Salt.
          -> a                  -- ^ Value to transform.
          -> Int
hashUsing f salt x = hashWithSalt salt (f x)

instance Hashable1 Maybe where
    liftHashWithSalt _ s Nothing = s `hashInt` 0
    liftHashWithSalt h s (Just a) = s `hashInt` distinguisher `h` a

instance Hashable a => Hashable1 (Either a) where
    liftHashWithSalt = defaultLiftHashWithSalt

instance Hashable2 Either where
    liftHashWithSalt2 h _ s (Left a) = s `hashInt` 0 `h` a
    liftHashWithSalt2 _ h s (Right b) = s `hashInt` distinguisher `h` b

instance Hashable a1 => Hashable1 ((,) a1) where
    liftHashWithSalt = defaultLiftHashWithSalt

instance Hashable2 (,) where
    liftHashWithSalt2 h1 h2 s (a1, a2) = s `h1` a1 `h2` a2

instance Hashable1 [] where
    liftHashWithSalt h salt arr = finalise (foldl' step (salt, 0) arr)
      where
        finalise (s, l) = hashWithSalt s l
        step (s, l) x   = (h s x, l + 1)

instance Hashable2 Const where
    liftHashWithSalt2 f _ salt (Const x) = f salt x

instance Hashable1 Proxy where
    liftHashWithSalt _ s _ = s

instance Hashable1 NE.NonEmpty where
    liftHashWithSalt h salt (a NE.:| as) = liftHashWithSalt h (h salt a) as
