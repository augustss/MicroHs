module FromIntegral(main) where
import Control.Exception
import Data.Int
import Data.List(nub)
import Data.Word
import Foreign.C.Types

-- Check that fromIntegral gives the same result as fromInteger . toInteger
-- for all pairs of types, on boundary values.

data Tgt = forall b . (Num b, Eq b) => Tgt String b

tgts :: [Tgt]
tgts =
  [ Tgt "Int" (0::Int), Tgt "Int8" (0::Int8), Tgt "Int16" (0::Int16), Tgt "Int32" (0::Int32), Tgt "Int64" (0::Int64)
  , Tgt "Word" (0::Word), Tgt "Word8" (0::Word8), Tgt "Word16" (0::Word16), Tgt "Word32" (0::Word32), Tgt "Word64" (0::Word64)
  , Tgt "Integer" (0::Integer), Tgt "CInt" (0::CInt), Tgt "CUInt" (0::CUInt), Tgt "CSize" (0::CSize)
  , Tgt "Double" (0::Double)
  ]

cands :: [Integer]
cands = nub $ [-1, 0, 1] ++ concat [ [b-1, b, b+1] | n <- [7, 8, 15, 16, 31, 32, 63, 64], b <- [-(2^n), 2^n-1, 2^n] ]

vals :: forall a . (Integral a, Bounded a) => [a]
vals = [ fromInteger i | i <- cands, i >= toInteger (minBound :: a), i <= toInteger (maxBound :: a) ]

same :: forall b . Eq b => b -> b -> IO Bool
same x y = do
  rx <- try (evaluate x)
  ry <- try (evaluate y)
  return $ case (rx, ry) of
    (Right a, Right b) -> a == b
    (Left a, Left b) -> show (a :: SomeException) == show (b :: SomeException)
    _ -> False

check :: forall a . Integral a => a -> Tgt -> IO [String]
check x (Tgt tn t) = do
  ok <- same (fromIntegral x `asTypeOf` t) (fromInteger (toInteger x) `asTypeOf` t)
  return (if ok then [] else [tn])

src :: forall a . (Integral a, Bounded a) => String -> a -> IO ()
src name _ = do
  bad <- sequence [ check x t | t <- tgts, x <- vals :: [a] ]
  putStrLn $ name ++ ": " ++ show (nub (concat bad))

main :: IO ()
main = do
  src "Int" (0::Int)
  src "Int8" (0::Int8)
  src "Int16" (0::Int16)
  src "Int32" (0::Int32)
  src "Int64" (0::Int64)
  src "Word" (0::Word)
  src "Word8" (0::Word8)
  src "Word16" (0::Word16)
  src "Word32" (0::Word32)
  src "Word64" (0::Word64)
  src "CInt" (0::CInt)
  src "CUInt" (0::CUInt)
  src "CSize" (0::CSize)
  print (fromIntegral (-1 :: Int8) :: Word8)
  print (fromIntegral (200 :: Word8) :: Int16)
  print (fromIntegral (-1 :: Int) :: Integer)
  print (fromIntegral (maxBound :: Word32) :: Integer)
  print (fromIntegral (maxBound :: Word16) :: Word32)
  print (fromIntegral (-5 :: CInt) :: Int)
