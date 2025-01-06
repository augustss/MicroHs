module MicroHs.MRnf where
import Prelude(); import MHSPrelude
import Data.Text
import Data.Version
import System.IO.MD5(MD5CheckSum)

class MRnf a where
  mrnf :: a -> ()
  mrnf a = seq a ()

instance MRnf Int
instance MRnf Char
instance (MRnf a, MRnf b) => MRnf (a, b) where
  mrnf (a, b) = mrnf a `seq` mrnf b
instance (MRnf a, MRnf b) => MRnf (Either a b) where
  mrnf (Left a) = mrnf a
  mrnf (Right a) = mrnf a
instance MRnf a => MRnf [a] where
  mrnf [] = ()
  mrnf (x:xs) = mrnf x `seq` mrnf xs
instance MRnf a => MRnf (Maybe a) where
  mrnf Nothing = ()
  mrnf (Just x) = mrnf x
instance MRnf Text
instance MRnf Version where
  mrnf v = mrnf (versionBranch v)
instance MRnf Rational where
  mrnf x = (x == 0) `seq` ()
instance MRnf Double
instance MRnf Integer where
  mrnf x = (x == 0) `seq` ()
instance MRnf Bool
instance MRnf (a -> b)
instance MRnf MD5CheckSum  -- Not quite NF, but close
