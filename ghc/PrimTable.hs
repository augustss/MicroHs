module PrimTable(module PrimTable) where
import Data.Char
import Data.Maybe
import System.IO
import Unsafe.Coerce
import GHC.Types(Any)

primitive :: String -> Any
primitive s = fromMaybe (error $ "primitive: " ++ s) $ lookup s primOps

newtype DIO a = DIO { unDIO :: IO a }

primOps :: [(String, Any)]
primOps =
  [ comb "S" (\ f g x -> f x (g x))
  , comb "S'" (\ k f g x -> k f x (g x))
  , comb "K" (\ x _y -> x)
  , comb "A" (\ _x y -> y)
  , comb "T" (\ x y -> y x)
  , comb "I" (\ x -> x)
  , comb "Y" (\ f -> let r = f r in r)
  , comb "B" (\ f g x -> f (g x))
  , comb "B'" (\ k f g x -> k f (g x))
  , comb "BK" (\ f g _x -> f g)
  , comb "C" (\ f g x -> f x g)
  , comb "C'" (\ k f g x -> k f x g)
  , comb "P" (\ x y f -> f x y)
  , comb "O" (\ x y _g f -> f x y)

  , arith "+" (+)
  , arith "-" (-)
  , arith "*" (*)
  , arith "quot" quot
  , arith "rem" rem
  , arith "subtract" subtract
  , farith "fadd" (+)
  , farith "fsub" (-)
  , farith "fmul" (*)
  , cmp "feq" (==)
  , cmp "fne" (/=)
  , cmp "flt" (<)
  , cmp "fle" (<=)
  , cmp "fgt" (>)
  , cmp "fge" (>=)
  , comb "fshow" (show :: Double -> String)
  , cmp "==" (==)
  , cmp "/=" (/=)
  , cmp "<"  (<)
  , cmp "<=" (<=)
  , cmp ">"  (>)
  , cmp ">=" (>=)
  , cmp "error" err
  , comb "ord" ord
  , comb "chr" chr
  , comb "IO.>>=" iobind
  , comb "IO.>>" iothen
  , comb "IO.return" ioret
--  , comb "IO.getChar" getc
  , comb "IO.putChar" putc
  , comb "IO.stdin" stdin
  , comb "IO.stdout" stdout
  , comb "IO.stderr" stderr
  ]
  where
    comb n f = (n, unsafeCoerce f)
    arith :: String -> (Int -> Int -> Int) -> (String, Any)
    arith = comb
    farith :: String -> (Double -> Double -> Double) -> (String, Any)
    farith = comb
    cmp :: String -> (Int -> Int -> Bool) -> (String, Any)
    cmp n f = comb n (\ x y -> if f x y then cTrue else cFalse)
    cTrue _x y = y
    cFalse x _y = x
    iobind :: DIO a -> (a -> DIO b) -> DIO b
    iobind a k = DIO (unDIO a >>= \ x -> unDIO (k x))
    iothen :: DIO a -> DIO b -> DIO b
    iothen a b = DIO (unDIO a >> unDIO b)
    ioret :: a -> DIO a
    ioret a = DIO (return a)
--    getc h = undefined -- fromEnum <$> hGetChar h  -- XXX
    putc :: Handle -> Int -> DIO ()
    putc h c = DIO $ do
--      let h = unsafeCoerce hh :: Handle
--          c = unsafeCoerce cc :: Int
--      print (h, c)
      hPutChar h (toEnum c)
--    open = undefined
--    close = undefined
--    isnull = undefined

    err _ = error "ERROR"
