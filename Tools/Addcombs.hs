module Addcombs(main) where
import Prelude
import Data.Char
import System.Environment

chunkify :: Int -> [Char] -> [[Char]]
chunkify n [] = []
chunkify n xs =
  let (as, bs) = splitAt n xs
  in  as : chunkify n bs

showChunk :: [Char] -> String
showChunk = concatMap (\ c -> show (ord c) ++ ",")

main :: IO ()
main = do
{-
  args <- getArgs
  let fn = case args of { [a] -> a; _ -> error "Usage: Addcombs file" }
  file <- readFile fn
-}
  hSetBinaryMode stdin True
  file <- hGetContents stdin
  let size = length file
      chunks = chunkify 20 file
  putStrLn $ "unsigned char combexprdata[] = {"
  mapM_ (putStrLn . showChunk) chunks
  putStrLn "0 };"
  putStrLn "unsigned char *combexpr = combexprdata;"
  putStrLn $ "int combexprlen = " ++ show size ++ ";"
