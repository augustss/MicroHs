module Error88() where
import Data.Kind(Constraint)
type Id (c :: Constraint) = c
instance Id c
