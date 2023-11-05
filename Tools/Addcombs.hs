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
  putStrLn $ "struct { BFILE mets; size_t b_size; size_t b_pos; uint8_t b_buffer[]; } combs =\n { { getb_buf, ungetb_buf, closeb_buf }, "
             ++ show size ++ ", 0, {"
  mapM_ (putStrLn . showChunk) chunks
  putStrLn "}};"
  putStrLn "BFILE *comb_internal = (BFILE*)&combs;"
