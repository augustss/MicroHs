module Error108() where
import Data.STRef
data Ex = forall r. Ex (STRef r Int)
wrong :: Ex -> forall r. STRef r Int
wrong (Ex ref) = ref
