module Numeric(
  showSigned,
    showIntAtBase,
    showInt,
    showBin,
    showHex,
    showOct,
    
    readSigned,
    readInt,
    readBin,
    readDec,
    readOct,
    readHex,
    ) where
import Primitives
import Control.Error
import Control.Monad
import Data.Bool
import Data.Char
import Data.Eq
import Data.Integral
import Data.List
import Data.Num
import Data.Ord
import Text.Read(ReadS, readParen)
import Text.Show(ShowS, showChar)

readInt :: forall a . Num a => a -> (Char -> Bool)  -> (Char -> Int) -> ReadS a
readInt base isDig valDig s = do
  (c, cs) <- lex s
  guard (isDig c)
  let loop r (c:cs) | isDig c = loop (r * base + fromIntegral (valDig c)) cs
      loop r ds = return (r, ds)
  loop (fromIntegral (valDig c)) cs

readBin :: forall a . (Num a) => ReadS a
readBin = readInt 2 isBinDigit digitToInt

isBinDigit :: Char -> Bool
isBinDigit c = c `primCharEQ` '0' || c `primCharEQ` '1'

readOct :: forall a . (Num a) => ReadS a
readOct = readInt 8 isOctDigit digitToInt

readDec :: forall a . (Num a) => ReadS a
readDec = readInt 10 isDigit digitToInt

readHex :: forall a . (Num a) => ReadS a
readHex = readInt 16 isDigit digitToInt

readSigned :: forall a . (Num a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
  where
    read' :: ReadS a
    read' r  = readPos r ++
                do
                  -- XXX compiler broken ('-',s) <- lex r
                  (c,s) <- lex r
                  guard (c == '-')
                  (x, t) <- readPos s
                  return (- x, t)


-- Really bad lexer
lex :: ReadS Char
lex "" = []
lex (c:cs) | isSpace c = lex cs
           | True = [(c, cs)]

-------------------------------------------------------------------------------

showSigned :: forall a . (Ord a, Integral a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p n r
    | n < 0 =
      if p > (6::Int) then
        '(' : '-' : showPos n (')' : r)
      else
        '-' : showPos n r
    | otherwise = showPos n r

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
showIntAtBase :: forall a . (Ord a, Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr an
  | base <= 1 = error "Numeric.showIntAtBase: unsupported base"
  | otherwise = showNeg (- an)
   where
    showNeg n | (n > 0) = error "Numeric.showIntAtBase: negative argument"
              | otherwise = showNeg' n
    -- Some trickery to show minBound correctly.
    -- To print the number n, pass -n.
    showNeg' n r =
      let
        c = toChr (fromIntegral (- rem n base))
      in  if n > - base then
            c : r
          else
            showNeg' (quot n base) (c : r)

showInt :: forall a . (Ord a, Integral a) => a -> ShowS
showInt = showIntAtBase 10 intToDigit

showHex :: forall a . (Ord a, Integral a) => a -> ShowS
showHex = showIntAtBase 16 intToDigit

showOct :: forall a . (Ord a, Integral a) => a -> ShowS
showOct = showIntAtBase 8  intToDigit

showBin :: forall a . (Ord a, Integral a) => a -> ShowS
showBin = showIntAtBase 2  intToDigit
