module Error107(main) where

instance Show Int where show _ = "xxx"

main :: IO ()
main = print (1::Int)
