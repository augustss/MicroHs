module Base64 where
import System.Environment
import System.IO
import System.IO.Transducers

main :: IO ()
main = do
  [fn] <- getArgs
  file <- readBinaryFile fn
  hout <- openFile "out.b64" WriteMode
  hout' <- addBase64 hout
  hPutStr hout' file
  hClose hout'

  hin <- openBinaryFile "out.b64" ReadMode
  hin' <- addBase64 hin
  ofile <- hGetContents hin'
  writeBinaryFile "out" ofile
