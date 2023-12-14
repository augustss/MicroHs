module PrimTable(module PrimTable) where
import Data.Char
import Data.Maybe
import System.IO
import Unsafe.Coerce
import GHC.Types(Any)
import Debug.Trace

primitive :: String -> Any
primitive s | trace ("primitive " ++ show s) False = undefined
primitive "dynsym" = unsafeCoerce dynsym
primitive s = fromMaybe (error $ "primitive: " ++ s) $ lookup s primOps

primOps :: [(String, Any)]
primOps =
  [ comb "S" (\ f g x -> f x (g x))
  , comb "K" (\ x _y -> x)
  , comb "I" (\ x -> x)
  , comb "B" (\ f g x -> f (g x))
  , comb "C" (\ f g x -> f x g)
  , comb "S'" (\ k f g x -> k (f x) (g x))
  , comb "B'" (\ k f g x -> k f (g x))
  , comb "C'" (\ k f g x -> k (f x) g)
  , comb "A" (\ _x y -> y)
  , comb "U" (\ x y -> y x)
  , comb "Y" (\ f -> let r = f r in r)
  , comb "Z" (\ f g _x -> f g)
  , comb "P" (\ x y f -> f x y)
  , comb "R" (\ x y f -> y f x)
  , comb "O" (\ x y _g f -> f x y)
  , comb "K2" (\ x _y _z -> x)
  , comb "K3" (\ x _y _z _w -> x)
  , comb "K4" (\ x _y _z _w _v -> x)

  , arith "+" (+)
  , arith "-" (-)
  , arith "*" (*)
  , arith "quot" quot
  , arith "rem" rem
  , arith "subtract" subtract
  , farith "fadd" (+)
  , farith "fsub" (-)
  , farith "fmul" (*)
  , cmp "==" (==)
  , cmp "/=" (/=)
  , cmp "<"  (<)
  , cmp "<=" (<=)
  , cmp ">"  (>)
  , cmp ">=" (>=)
  , fcmp "feq" (==)
  , fcmp "fne" (/=)
  , fcmp "flt" (<)
  , fcmp "fle" (<=)
  , fcmp "fgt" (>)
  , fcmp "fge" (>=)
  , comb "fshow" (show :: Double -> String)
  , comb "error" err
  , comb "ord" ord
  , comb "chr" chr
  , comb "IO.>>=" iobind
  , comb "IO.>>" iothen
  , comb "IO.return" ioret
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
    fcmp :: String -> (Double -> Double -> Bool) -> (String, Any)
    fcmp n f = comb n (\ x y -> if f x y then cTrue else cFalse)
    cTrue _x y = y
    cFalse x _y = x
    iobind :: IO a -> (a -> IO b) -> IO b
    iobind = (>>=)
    iothen :: IO a -> IO b -> IO b
    iothen = (>>)
    ioret :: a -> IO a
    ioret = return
    putc :: Int -> IO ()
    putc c = hPutChar stdout (chr c)

    err _ = error "ERROR"

dynsym :: String -> Any
dynsym cfun = error $ "dynsym " ++ cfun
