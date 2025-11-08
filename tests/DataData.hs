module DataData where
import Data.Functor.Identity
import Data.Data

data T a = C0 | C1 a | C2 Int a | C3 { x,y,z :: a } | Int `C4` Int
  deriving (Data, Show)

main :: IO ()
main = do
  print $ dataTypeOf (C1 ())
  print $ toConstr (C1 ())
  let c1 = toConstr (C1 ())
      f1 :: Int -> T Int
      f1 = runIdentity (gunfold (<*>) pure c1)
  print ()

--  print $  True
