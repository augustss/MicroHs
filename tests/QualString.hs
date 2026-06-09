module QualString where

fromString :: String -> String
fromString = reverse

main :: IO ()
main = do
  putStrLn QualString."Hello World!"
