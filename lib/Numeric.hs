module Numeric(
  showSigned,
    showIntAtBase,
    showInt,
    showBin,
    showHex,
    showOct,
    showIntegral,
    
    readSigned,
    readInt,
    readBin,
    readDec,
    readOct,
    readHex,
    readIntegral,
    ) where
import Primitives
import Control.Error
import Control.Monad
import Control.Monad.Fail
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
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
  loop 0 (c:cs)

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
                  ('-',s) <- lex r
                  (x, t) <- readPos s
                  return (- x, t)

readIntegral :: forall a . (Integral a) => Int -> ReadS a
readIntegral _ = readSigned readAny
  where readAny ('0':'x':cs) = readHex cs            -- XXX not quite right, allows space after 'x'
        readAny ('0':'o':cs) = readOct cs            -- XXX not quite right, allows space after 'x'
        readAny ('0':'b':cs) = readBin cs            -- XXX not quite right, allows space after 'x'
        readAny cs = readDec cs

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
        '(' : '-' : showPos (-n) (')' : r)
      else
        '-' : showPos (-n) r
    | otherwise = showPos n r

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
-- If the argument, n, is <0 and -n == n (i.e., n == minBound) it will
-- return the string for (abs n).
showIntAtBase :: forall a . (Ord a, Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr an
  | base <= 1 = error "Numeric.showIntAtBase: unsupported base"
  | an < 0 =
    if -an < 0 then
      -- We are at minBound
      showPos (- quot an base) . showPos (- rem an base)
    else
      error "Numeric.showIntAtBase: negative argument"
  | otherwise = showPos an
   where
    showPos n r =
      let
        c = toChr (fromIntegral (rem n base))
      in  c `seq`
          if n < base then
            c : r
          else
            showPos (quot n base) (c : r)

showInt :: forall a . (Ord a, Integral a) => a -> ShowS
showInt = showIntAtBase 10 intToDigit

showHex :: forall a . (Ord a, Integral a) => a -> ShowS
showHex = showIntAtBase 16 intToDigit

showOct :: forall a . (Ord a, Integral a) => a -> ShowS
showOct = showIntAtBase 8  intToDigit

showBin :: forall a . (Ord a, Integral a) => a -> ShowS
showBin = showIntAtBase 2  intToDigit

showIntegral :: forall a . (Ord a, Integral a) => Int -> a -> ShowS
showIntegral = showSigned showInt
