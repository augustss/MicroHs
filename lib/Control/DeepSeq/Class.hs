-- Copyright 2025 Lennart Augustsson
-- See LICENSE file for full license.
module Control.DeepSeq.Class(module Control.DeepSeq.Class) where
import Prelude()
import Primitives(primSeq)
import Data.Bool
import Data.Char
import Data.Eq
import Data.Float
import Data.Double
import Data.Either
import Data.Function
import Data.Int
import Data.Integer
import Data.Integral
import Data.List_Type
import Data.Maybe_Type
import Data.Ord
import Data.Tuple
import Data.Word
import Mhs.Builtin

-- NFData class and instances for primitive types.

class NFData a where
  rnf :: a -> ()
  rnf a = seq a ()

infixr 0 `deepseq`
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

infixr 0 $!!
($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x

force :: (NFData a) => a -> a
force x = x `deepseq` x

instance NFData Int
instance NFData Word
instance NFData Float
instance NFData Double
instance NFData Char
instance NFData Bool
instance NFData Ordering
instance NFData ()

instance NFData Integer where
  rnf x = (x == 0) `seq` ()

instance NFData a => NFData (Maybe a) where
  rnf Nothing = ()
  rnf (Just a) = rnf a

instance NFData a => NFData [a] where
  rnf [] = ()
  rnf (x:xs) = rnf x `seq` rnf xs

instance (NFData a, NFData b) => NFData (Either a b) where
  rnf (Left a) = rnf a
  rnf (Right b) = rnf b

instance (NFData a, NFData b) => NFData (a, b) where
  rnf (a, b) = rnf a `seq` rnf b

