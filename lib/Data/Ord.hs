module Data.Ord(
  Ordering(..),
  eqOrdering,
  isEQ,
  ) where
import Data.Bool_Type
import Data.Ordering_Type
import Data.Int

isEQ :: Ordering -> Bool
isEQ EQ = True
isEQ _  = False

eqOrdering :: Ordering -> Ordering -> Bool
eqOrdering LT LT = True
eqOrdering EQ EQ = True
eqOrdering GT GT = True
eqOrdering _  _  = False
