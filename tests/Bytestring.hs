module Bytestring where
import Data.Word
import Data.ByteString

bs1 :: ByteString
bs1 = pack [1,2,3]

bs2 :: ByteString
bs2 = pack [1,2,4]

bs3 :: ByteString
bs3 = pack [1,2]

main :: IO ()
main = do
  print (unpack bs1)
  print bs1
  print $ bs1 `append` bs2
  print [ op x y | op <- [(==), (/=), (<), (<=), (>), (>=)]
                 , x <- [bs1, bs2, bs3]
                 , y <- [bs1, bs2, bs3]
        ]
