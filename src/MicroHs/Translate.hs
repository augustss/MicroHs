module MicroHs.Translate where
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import GHC.Types
import Unsafe.Coerce
import System.IO
import System.IO.Unsafe

import MicroHs.Desugar(LDef)
import MicroHs.Parse(Ident)
import MicroHs.Exp

data DIO a = DIO { unDIO :: IO a }

translate :: (Ident, [LDef]) -> IO ()
translate (mainName, ds) =
  let m :: M.Map Ident Any
      m = M.fromList [(n, trans look d) | (n, d) <- ds ]
      look n = fromMaybe (error $ "not found " ++ n) $ M.lookup n m
  in  unDIO $ unsafeCoerce $ look mainName

--aa :: Any -> (Any -> Any)
--aa = unsafeCoerce

trans :: (Ident -> Any) -> Exp -> Any
trans r (Var n) = r n
trans r (App f a) = unsafeCoerce (trans r f) (trans r a)
trans _ (Int i) = unsafeCoerce i
trans _ (Prim p) = fromMaybe (error $ "Prim " ++ p) $ lookup p primOps
trans _ e = error $ "impossible: " ++ show e

primOps :: [(String, Any)]
primOps =
  [ comb "S" (\ f g x -> f x (g x))
  , comb "K" (\ x _y -> x)
  , comb "I" (\ x -> x)
  , comb "C" (\ f g x -> f x g)
  , comb "B" (\ f g x -> f (g x))
  , comb "T" (\ _x y -> y)
  , comb "Y" (\ f -> let r = f r in r)
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
    cmp :: String -> (Int -> Int -> Bool) -> (String, Any)
    cmp n f = comb n (\ x y -> if f x y then cTrue else cFalse)
    cTrue _x y = y
    cFalse x _y = x
    iobind :: DIO a -> (a -> DIO b) -> DIO b
    iobind a k = unDIO a >>= k
    iothen :: DIO a -> DIO b -> DIO b
    iothen a b = unDIO a >> b
    ioret :: a -> DIO a
    ioret = DIO . return
--    getc h = undefined -- fromEnum <$> hGetChar h  -- XXX
    putc :: Handle -> Int -> DIO ()
    putc h c = do
--      let h = unsafeCoerce hh :: Handle
--          c = unsafeCoerce cc :: Int
      print (h, c)
      DIO $ hPutChar h (toEnum c)
--    open = undefined
--    close = undefined
--    isnull = undefined

    err _ = error "ERROR"

