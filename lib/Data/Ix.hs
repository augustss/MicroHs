module Data.Ix(Ix(..)) where
import qualified Prelude()
import Control.Error
import Data.Bool
import Data.Char
import Data.Enum
import Data.Eq
import Data.Int
import Data.Integer
import Data.Integral
import Data.List
import Data.Num
import Data.Ord
import Data.Tuple
import Data.Word
import {-# SOURCE #-} Data.Typeable
import System.IO.Internal (IOMode(..))

class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  unsafeIndex :: (a, a) -> a -> Int
  unsafeRangeSize :: (a, a) -> Int

  index b i | inRange b i = unsafeIndex b i
            | otherwise   = error "Ix.index: out of bound"

  rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
                     | otherwise   = 0

  unsafeRangeSize b@(_l,h) = unsafeIndex b h + 1

instance Ix Int where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = i - m
  inRange (m, n) i = m <= i && i <= n

instance Ix Int8 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Int16 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Int32 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Int64 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Integer where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromInteger (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Word where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Word8 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Word16 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Word32 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Word64 where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

instance Ix Bool where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromEnum i - fromEnum m
  inRange (m, n) i = m <= i && i <= n

instance Ix Char where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromEnum i - fromEnum m
  inRange (m, n) i = m <= i && i <= n

instance Ix Ordering where
  range (m,n) = [m..n]
  unsafeIndex (m,_n) i = fromEnum i - fromEnum m
  inRange (m, n) i = m <= i && i <= n

deriving instance Ix GeneralCategory

-- XXX
--deriving instance Ix SeekMode

deriving instance Ix IOMode

instance Ix () where
  range ((),()) = [()]
  unsafeIndex ((),_n) () = 0
  inRange ((), ()) () = True

instance Ix a => Ix (Solo a) where -- as derived
  range (MkSolo l, MkSolo u) =
    [ MkSolo i | i <- range (l,u) ]
  unsafeIndex (MkSolo l, MkSolo u) (MkSolo i) =
    unsafeIndex (l,u) i
  inRange (MkSolo l, MkSolo u) (MkSolo i) =
    inRange (l, u) i

instance (Ix a, Ix b) => Ix (a, b) where -- as derived
  range ((l1,l2),(u1,u2)) =
    [ (i1,i2) | i1 <- range (l1,u1), i2 <- range (l2,u2) ]
  unsafeIndex ((l1,l2),(u1,u2)) (i1,i2) =
    unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2
  inRange ((l1,l2),(u1,u2)) (i1,i2) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2

instance (Ix a1, Ix a2, Ix a3) => Ix (a1, a2, a3)  where
  range ((l1,l2,l3),(u1,u2,u3)) =
    [(i1,i2,i3) | i1 <- range (l1,u1),
                  i2 <- range (l2,u2),
                  i3 <- range (l3,u3)]

  unsafeIndex ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1)

  inRange ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3

instance (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1, a2, a3, a4)  where
  range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
    [(i1,i2,i3,i4) | i1 <- range (l1,u1),
                     i2 <- range (l2,u2),
                     i3 <- range (l3,u3),
                     i4 <- range (l4,u4)]

  unsafeIndex ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
    unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1))

  inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3 && inRange (l4,u4) i4

instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) => Ix (a1, a2, a3, a4, a5) where
  range ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) =
    [(i1,i2,i3,i4,i5) | i1 <- range (l1,u1),
                        i2 <- range (l2,u2),
                        i3 <- range (l3,u3),
                        i4 <- range (l4,u4),
                        i5 <- range (l5,u5)]

  unsafeIndex ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
    unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
    unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1)))

  inRange ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
    inRange (l5,u5) i5
