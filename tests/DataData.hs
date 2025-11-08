module DataData where
import Data.Functor.Const
import Data.Data

data T a = C0 | C1 a | C2 Int a | C3 { x,y,z :: a } | Int `C4` Int
  deriving (Data, Show)

showConstr' :: Constr -> String
showConstr' c =
  "Constr { conrep=" ++ show (constrRep c)
  ++     ", constring=" ++ show c
  ++     ", confields=" ++ show (constrFields c)
  ++     ", confixity=" ++ show (constrFixity c)
  ++     ", datatype="  ++ dataTypeName (constrType c)

arity :: Data a => a -> Int
arity x = getConst (gfoldl step start x)
  where
    start :: forall g. g -> Const Int g
    start _ = Const 0
    step  :: forall d b. Data d
          => Const Int (d -> b) -> d -> Const Int b
    step (Const n) _ = Const (n + 1)

main :: IO ()
main = do
  print $ dataTypeOf (C1 ())
  print $ toConstr (C1 ())
  print (arity (C1 ()))
  print (arity (C4 1 2 :: T ()))
  let AlgRep cs = dataTypeRep (dataTypeOf (C1 ()))
  mapM_ (putStrLn . showConstr') cs
