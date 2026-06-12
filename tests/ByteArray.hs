module ByteArray where
import Control.Monad.ST
import Data.Array.Byte

main :: IO ()
main = do
  let b1 = runST $ do
        a <- newMutableByteArray 5
        writeWord8 a 1 65
        writeWord8 a 3 66
        c <- readWord8 a 1
        writeWord8 a 4 c
        freezeMutableByteArray a
  print b1

  let b2 = runST $ do
        a <- newMutableEmptyByteArray 2
        appendByte a 110
        appendByte a 111
        appendByte a 112
        appendByte a 113
        appendByte a 114
        appendChar a 'A'
        appendChar a 'å'
        freezeMutableByteArray a
  print b2
