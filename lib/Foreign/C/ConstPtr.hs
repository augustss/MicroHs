module Foreign.C.ConstPtr (ConstPtr(..)) where
import Foreign.Ptr
import Foreign.Storable

newtype ConstPtr a = ConstPtr { unConstPtr :: Ptr a }
  deriving (Eq, Ord)
  deriving newtype (Storable)

-- doesn't use record syntax
instance Show (ConstPtr a) where
  showsPrec d (ConstPtr p) = showParen (d > 10) $ showString "ConstPtr " . showsPrec 11 p
