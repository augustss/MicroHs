module Error109() where
import Control.Monad.ST
import Data.STRef
data Ex = forall r. Ex (STRef r Int)
wrong2 :: Ex -> forall r. ST r Int
wrong2 (Ex ref) = readSTRef ref
