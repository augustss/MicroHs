module Eq(main) where
import Prelude
import Data.Double()

main :: IO ()
main = do
  putStrLn $ show [1==1, 'a'=='a', 1.1==1.1,
                   True==True, False==False,
                   (Nothing::Maybe Int)==Nothing, Just 1 == Just 1,
                   [1,2,3] == [1,2,3],
                   (1,2) == (1,2),
                   (Left 1 :: Either Int Char) == Left 1, (Right 'a' :: Either Int Char) == Right 'a'
                  ]
  putStrLn $ show [1==2, 'a'=='b', 1.1==1.2,
                   True==False, False==True,
                   Nothing==Just 1, Just 1 == Nothing,
                   [1,2,3] == [1,2,4],
                   (1,2) == (1,4),
                   Left 1 == Right 'a', Right 'a' == Left 1
                  ]

