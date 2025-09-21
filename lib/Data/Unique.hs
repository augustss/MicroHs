module Data.Unique(
  Unique,
  newUnique,
  hashUnique,
  ) where
import Data.IORef
import System.IO.Unsafe

newtype Unique = Unique Integer deriving (Eq, Ord)

uniqSource :: IORef Integer
uniqSource = unsafePerformIO (newIORef 0)

newUnique :: IO Unique
newUnique = do
  r <- atomicModifyIORef' uniqSource $ \x -> let z = x+1 in (z,z)
  return (Unique r)

hashUnique :: Unique -> Int
hashUnique (Unique i) = fromInteger i
