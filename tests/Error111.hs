module Error111(main) where

main :: IO ()
main = putStrLn "abc\x110000"
