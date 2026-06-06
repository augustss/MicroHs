-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.MakeCArray(makeCArray) where
import qualified Prelude(); import MHSPrelude
import Data.Char
import qualified Data.ByteString as BS
import MicroHs.Flags
import qualified System.Compress as C

chunkify :: Int -> String -> [String]
chunkify _ [] = []
chunkify n xs =
  let (as, bs) = splitAt n xs
  in  as : chunkify n bs

showChunk :: [Char] -> String
showChunk = concatMap (\ c -> show (ord c) ++ ",")

showBlobChunk :: BS.ByteString -> String
showBlobChunk bs = concatMap (\ b -> show b ++ ",") (BS.unpack bs)

makeCArray :: Flags -> [BS.ByteString] -> String -> String
makeCArray flags blobs file =
  let fileCArray =
        if compress flags then
          makeCArray' ('z' : C.compress file)
        else
          makeCArray' file
  in  fileCArray <> makeBlobArray blobs

makeCArray' :: String -> String
makeCArray' file =
  let chunks = chunkify 20 file
  in  unlines $ ["static const unsigned char data[] = {"] ++
                map showChunk chunks ++
                ["};",
                 "const unsigned char *combexpr = data;",
                 "const int combexprlen = " ++ show (length file) ++ ";"
                ]

makeBlobArray :: [BS.ByteString] -> String
makeBlobArray [] =
  unlines ["const unsigned char *blobptrs = 0;",
           "const int *bloboffs = 0;",
           "const int blobcount = 0;"]
makeBlobArray blobs =
  let offsets = scanl (+) 0 (map BS.length blobs)
      allBytes = BS.concat blobs
      chunks = chunkifyBS 20 allBytes
  in  unlines $ ["static const unsigned char blobdata[] = {"] ++
                map showBlobChunk chunks ++
                ["};",
                 "static const int blob_offsets[] = {" ++ concatMap (\ o -> show o ++ ",") offsets ++ "};",
                 "const unsigned char *blobptrs = blobdata;",
                 "const int *bloboffs = blob_offsets;",
                 "const int blobcount = " ++ show (length blobs) ++ ";"
                ]

chunkifyBS :: Int -> BS.ByteString -> [BS.ByteString]
chunkifyBS _ bs | BS.null bs = []
chunkifyBS n bs =
  let (as, bbs) = BS.splitAt n bs
  in  as : chunkifyBS n bbs
