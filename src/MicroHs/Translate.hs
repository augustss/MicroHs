module MicroHs.Translate where
import Data.Char
import Data.Maybe
import GHC.Types
import Unsafe.Coerce

import MicroHs.Exp

translate = trans

--aa :: Any -> (Any -> Any)
--aa = unsafeCoerce

trans :: Exp -> Any
trans (App f a) = unsafeCoerce (trans f) (trans a)
trans (Int i) = unsafeCoerce i
trans (Prim p) = fromMaybe (error $ "Prim " ++ p) $ lookup p primOps
trans e = error $ "impossible: " ++ show e

primOps :: [(String, Any)]
primOps =
  [ comb "S" (\ f g x -> f x (g x))
  , comb "K" (\ x _y -> x)
  , comb "I" (\ x -> x)
  , comb "C" (\ f g x -> f x g)
  , comb "B" (\ f g x -> f (g x))
  , comb "T" (\ _x y -> y)
  , comb "Y" (let fix f = f (fix f) in fix)
  , comb "P" (\ x y f -> f x y)
  , comb "O" (\ x y _g f -> f x y)
  , comb "S'" (\ k f g x -> k f x (g x))
  , comb "B'" (\ k f g x -> k f (g x))
  , comb "C'" (\ k f g x -> k f x g)
  , arith "+" (+)
  , arith "-" (-)
  , arith "*" (*)
  , arith "quot" quot
  , arith "rem" rem
  , arith "subtract" subtract
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
  , comb "IO.h
  ]
  where
    comb n f = (n, unsafeCoerce f)
    arith :: String -> (Int -> Int -> Int) -> (String, Any)
    arith = comb
    cmp :: String -> (Int -> Int -> Bool) -> (String, Any)
    cmp n f = comb n (\ x y -> if f x y then cTrue else cFalse)
    cTrue _x y = y
    cFalse x _y = x
    iobind :: IO a -> (a -> IO b) -> IO b
    iobind = (>>=)
    iothen :: IO a -> IO b -> IO b
    iothen = (>>)
    ioret :: a -> IO a
    ioret = return

    err _ = error "ERROR"

{-

  case IO_GETCHAR: fprintf(f, "$IO.getChar"); break;
  case IO_PUTCHAR: fprintf(f, "$IO.putChar"); break;
  case IO_OPEN: fprintf(f, "$IO.open"); break;
  case IO_CLOSE: fprintf(f, "$IO.close"); break;
  case IO_ISNULLHANDLE: fprintf(f, "$IO.isNullHandle"); break;
-}
