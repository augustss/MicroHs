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
  args <- getArgs
  file <- readFile (head args)
  let size = length file
      chunks = chunkify 20 file
  putStrLn $ "struct { size_t b_size; size_t b_pos; uint8_t b_buffer[]; } combs = { " ++ show size ++ ", 0, {"
  mapM_ (putStrLn . showChunk) chunks
  putStrLn "}};"
  putStrLn "BFILE *comb_internal = (BFILE*)&combs;"
