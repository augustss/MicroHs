-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.MakeCArray(makeCArray) where
import qualified Prelude(); import MHSPrelude
import Data.Char
import MicroHs.Flags
import qualified System.Compress as C

chunkify :: Int -> String -> [String]
chunkify _ [] = []
chunkify n xs =
  let (as, bs) = splitAt n xs
  in  as : chunkify n bs

showChunk :: [Char] -> String
showChunk = concatMap (\ c -> show (ord c) ++ ",")

makeCArray :: Flags -> String -> String
makeCArray flags file =
  if compress flags then
    makeCArray' ('z' : C.compress file)  -- stick a 'z' in front to indicate compressed
  else
    makeCArray' file

makeCArray' :: String -> String
makeCArray' file =
  let chunks = chunkify 20 file
  in  unlines $ ["static const unsigned char data[] = {"] ++
                map showChunk chunks ++
                ["};",
                 "const unsigned char *combexpr = data;",
                 "const int combexprlen = " ++ show (length file) ++ ";"
                ]
