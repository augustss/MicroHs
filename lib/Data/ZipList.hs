module Data.ZipList(ZipList(..)) where
--import qualified Prelude(); import MiniPrelude
import Control.Applicative
import Data.Foldable
import Data.Traversable
--import Mhs.Builtin

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Ord, Show, Foldable)

instance Functor ZipList where
  fmap f (ZipList as) = ZipList (map f as)

instance Applicative ZipList where
  pure a = ZipList (repeat a)
  liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

instance Alternative ZipList where
   empty = ZipList []
   ZipList xs0 <|> ZipList ys0 = ZipList $ go xs0 ys0
     where
       go (x:xs) (_:ys) = x : go xs ys
       go    []     ys  = ys
       go    xs      _  = xs

instance Traversable ZipList where
    traverse f (ZipList x) = ZipList `fmap` traverse f x
