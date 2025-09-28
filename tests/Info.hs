module Info(main) where
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.Word

foreign import capi "want_gmp" want_gmp :: Int

thisOS :: String
thisOS | _isWindows = "Windows"
       | _isMacOS   = "MacOS"
       | _isLinux   = "Linux"
       | otherwise  = "Unknown OS"

main :: IO ()
main = do
  putStrLn $ "Running on " ++ thisOS
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

  putStrLn $ "GMP: " ++ if want_gmp /= 0 then "yes" else "no"
