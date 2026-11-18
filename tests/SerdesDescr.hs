module SerdesDescr(main) where

import Control.Monad(void)
import Data.Data
import System.IO
import System.IO.Serialize
import System.IO.Serialize.TypeDescr

data Shape = Circle Double | Rect Double Double
  deriving (Show, Data)

data Op = Op String (Int -> Int)
  deriving (Data)

write :: Data a => a -> IO ()
write a = withBinaryFile "descr.tmp" WriteMode $ \ h -> hSerializeDescr h a

readBack :: Data a => IO (Maybe a)
readBack = withBinaryFile "descr.tmp" ReadMode hDeserializeDescr

main :: IO ()
main = do
  write [Circle 1, Rect 2 3]
  readBack >>= \ r -> print (r :: Maybe [Shape])
  readBack >>= \ r -> print (fmap length (r :: Maybe [Int]))

  -- Functions have a Data instance.
  write (\ x -> x * 2 + 1 :: Int)
  readBack >>= \ r -> print (fmap ($ 20) (r :: Maybe (Int -> Int)))
  readBack >>= \ r -> print (void (r :: Maybe (Int -> Bool)))

  write [Op "inc" (+ 1), Op "dbl" (* 2)]
  readBack >>= \ r -> print (fmap (map (\ (Op n o) -> (n, o 10))) (r :: Maybe [Op]))

  -- The description of a function type includes its argument and result types.
  case getTypeDescr (undefined :: Double -> Shape) of
    TypeDescr _ env -> print (map fst env)
