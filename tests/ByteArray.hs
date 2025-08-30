module ByteArray where
import Control.Monad.ST
import Data.Array.Byte

main :: IO ()
main = do
  let b = runST $ do
        a <- newMutableByteArray 5
        writeWord8 a 1 65
        writeWord8 a 3 66
        freezeMutableByteArray a
  print b
