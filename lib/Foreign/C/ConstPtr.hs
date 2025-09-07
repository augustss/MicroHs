module Foreign.C.ConstPtr (ConstPtr(..), unConstPtr) where
import Foreign.Ptr

newtype ConstPtr a = ConstPtr (Ptr a)
  deriving (Eq, Ord, Show)

unConstPtr :: ConstPtr a -> Ptr a
unConstPtr (ConstPtr p) = p

