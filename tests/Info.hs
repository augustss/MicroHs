module Info(main) where
import Data.Version
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Info as I

foreign import capi "want_gmp" want_gmp :: Int

main :: IO ()
main = do
  putStrLn $ "Running on " ++ I.os ++ ", " ++ I.arch
  putStrLn $ "Compiler " ++ I.compilerName ++ "-" ++ showVersion I.fullCompilerVersion
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
      _ -> "mystery endian"

  putStrLn $ "GMP: " ++ if want_gmp /= 0 then "yes" else "no"
