-- Instance declarations that can't be put where
-- due to import cycles.
module Data.Orphans where
import qualified Prelude(); import MiniPrelude
import Data.Bits
import Data.Eq
import Data.Floating
import Data.Fractional
import Data.Ix
import Data.Num
import Data.Monoid
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Foreign.Storable
import Mhs.Builtin
import Text.Read

deriving instance (Num a) => Num (Down a)
deriving instance (Semigroup a) => Semigroup (Down a)
deriving instance (Monoid a) => Monoid (Down a)
deriving instance (Bits a) => Bits (Down a)
deriving instance (FiniteBits a) => FiniteBits (Down a)
deriving instance (Floating a) => Floating (Down a)
deriving instance (Fractional a) => Fractional (Down a)
deriving instance (Ix a) => Ix (Down a)
deriving instance (Read a) => Read (Down a)
deriving instance (Real a) => Real (Down a)
deriving instance (RealFrac a) => RealFrac (Down a)
deriving instance (RealFloat a) => RealFloat (Down a)
deriving instance (Show a) => Show (Down a)
deriving instance (Storable a) => Storable (Down a)

instance Num (f a) => Num (Alt f a) where
    Alt x + Alt y = Alt (x + y)
    Alt x * Alt y = Alt (x * y)
    fromInteger = Alt . fromInteger
    abs (Alt x) = Alt (abs x)
    negate (Alt x) = Alt (negate x)
    signum (Alt x) = Alt (signum x)
