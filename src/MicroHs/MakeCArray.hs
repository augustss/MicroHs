-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.MakeCArray(makeCArray) where
import Prelude
import Data.Char

chunkify :: Int -> String -> [String]
chunkify _ [] = []
chunkify n xs =
  let (as, bs) = splitAt n xs
  in  as : chunkify n bs

showChunk :: [Char] -> String
showChunk = concatMap (\ c -> show (ord c) ++ ",")

makeCArray :: String -> String
makeCArray file =
  let chunks = chunkify 20 file
  in  unlines $ ["static unsigned char data[] = {"] ++
                map showChunk chunks ++
                ["};",
                 "unsigned char *combexpr = data;",
                 "int combexprlen = " ++ show (length file) ++ ";"
                ]
