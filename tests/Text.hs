module Text where
import Data.Text

bs1 :: Text
bs1 = pack "abc"

bs2 :: Text
bs2 = pack "abd"

bs3 :: Text
bs3 = pack "ab"

bs4 :: Text
bs4 = pack "acd"

main :: IO ()
main = do
  print (unpack bs1)
  print bs1
  print $ bs1 `append` bs2
  print [ op x y | op <- [(==), (/=), (<), (<=), (>), (>=)]
                 , x <- [bs1, bs2, bs3, bs4]
                 , y <- [bs1, bs2, bs3, bs4]
        ]
  print [ compare x y | x <- [bs1, bs2, bs3, bs4], y <- [bs1, bs2, bs3, bs4] ]
  print ("abc" :: Text)
