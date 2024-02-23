module Info(main) where
import Prelude
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.Word

main :: IO ()
main = do
  putStrLn $ "Running on " ++ if _isWindows then "Windows" else "Unix"
  putStr $ show _wordSize ++ " bit words, "

  let
    w :: Word
    w = if _wordSize == 32 then 0x01000002 else 0x0100000000000002
  p <- new w
  b <- peek (castPtr p :: Ptr Word8)
  putStrLn $
    case b of
      1 -> "big endian"
      2 -> "little endian"
      _ -> "Mystery Endian"
