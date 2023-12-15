module PrimTable(module PrimTable) where
import Control.Exception
import Data.Bits
import Data.Char
import Data.Maybe
--import Data.Word
import System.IO
import Unsafe.Coerce
import GHC.Types(Any)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO.Unsafe
import Debug.Trace
import Compat

primitive :: String -> Any
--primitive s | trace ("primitive " ++ show s) False = undefined
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
  , arithu "neg" negate
  , arithu "inv" complement
  , arithw "uquot" quot
  , arithw "urem" rem
  , arithw "and" (.&.)
  , arithw "or" (.|.)
  , arithw "xor" xor
  , arithwi "shl" shiftL
  , arithwi "shr" shiftR
  , arith "ashr" shiftR
  , cmp "==" (==)
  , cmp "/=" (/=)
  , cmp "<"  (<)
  , cmp "<=" (<=)
  , cmp ">"  (>)
  , cmp ">=" (>=)
  , comb "icmp" (\ x y -> fromOrdering (compare (x::Int) y))

  , comb "scmp" (\ x y -> fromOrdering (compare (toString x) (toString y)))
  , comb "sequal" (\ x y -> if toString x == toString y then cTrue else cFalse)

  , farith "fadd" (+)
  , farith "fsub" (-)
  , farith "fmul" (*)
  , farith "fdiv" (/)
  , farithu "fneg" negate
  , fcmp "feq" (==)
  , fcmp "fne" (/=)
  , fcmp "flt" (<)
  , fcmp "fle" (<=)
  , fcmp "fgt" (>)
  , fcmp "fge" (>=)
  , comb "fshow" (show :: Double -> String)
  , comb "itof" (fromIntegral :: Int -> Double)

  , comb "seq" seq
  , comb "rnf" rnf
  , comb "error" err
  , comb "ord" ord
  , comb "chr" chr
  , comb "IO.>>=" iobind
  , comb "IO.>>" iothen
  , comb "IO.return" ioret
  , comb "IO.print" ioprint
  , comb "IO.performio" unsafePerformIO
  , comb "IO.serialize" ioserialize
  , comb "IO.deserialize" iodeserialize
  , comb "newCAStringLen" (fmap fromPair . newCAStringLen . toString)

  , comb0 "IO.stdin" stdin
  , comb0 "IO.stdout" stdout
  , comb0 "IO.stderr" stderr
  ]
  where
    comb0 n f = (n, unsafeCoerce f)
    comb n f = (n, unsafeCoerce f)
--    comb n f = (n, unsafeCoerce (\ x -> trace (seq x n) (f x)))
    arith :: String -> (Int -> Int -> Int) -> (String, Any)
    arith = comb
    arithw :: String -> (Word -> Word -> Word) -> (String, Any)
    arithw = comb
    arithwi :: String -> (Word -> Int -> Word) -> (String, Any)
    arithwi = comb
    arithu :: String -> (Int -> Int) -> (String, Any)
    arithu = comb
    farith :: String -> (Double -> Double -> Double) -> (String, Any)
    farith = comb
    farithu :: String -> (Double -> Double) -> (String, Any)
    farithu = comb
    cmp :: String -> (Int -> Int -> Bool) -> (String, Any)
    cmp n f = comb n (\ x y -> if f x y then cTrue else cFalse)
    fcmp :: String -> (Double -> Double -> Bool) -> (String, Any)
    fcmp n f = comb n (\ x y -> if f x y then cTrue else cFalse)

    err s = error $ "error: " ++ toString s

    iobind :: IO a -> (a -> IO b) -> IO b
    iobind = (>>=)
    iothen :: IO a -> IO b -> IO b
    iothen = (>>)
    ioret :: a -> IO a
    ioret = return
    -- Can't implement this
    ioprint :: Handle -> a -> IO ()
    ioprint h _ = hPutStrLn h "no IO.print"
    ioserialize :: Handle -> a -> IO ()
    ioserialize h _ = hPutStrLn h "no IO.serialize"
    iodeserialize :: Handle -> IO a
    iodeserialize _ = error "iodeserialize"

    -- Can't implement this
    rnf :: a -> ()
    rnf x = seq x ()

    cTrue _x y = y
    cFalse x _y = x

fromOrdering :: Ordering -> (Any -> Any -> Any -> Any)
fromOrdering LT = \ x _y _z -> x
fromOrdering EQ = \ _x y _z -> y
fromOrdering GT = \ _x _y z -> z

fromPair :: (a, b) -> Any
fromPair (x, y) = unsafeCoerce $ \ f -> f x y

toList :: Any -> [Int]
toList a = (unsafeCoerce a) [] (\ i is -> i : toList is)

toString :: Any -> String
toString = map chr . toList

dynsym :: Any -> Any
dynsym acfun =
  let s = toString acfun
  in
--      trace ("dynsym: " ++ show s) $
      fromMaybe (error $ "cops: " ++ s) $ lookup s cops

cops :: [(String, Any)]
cops =
  [ comb "fputc" fputc
  , comb "getTimeMilli" getTimeMilli
  , comb "fgetc" fgetc
  , comb "fopen" fopen
  , comb "free" free
  ]
  where
    comb n f = (n, unsafeCoerce f)

    fputc :: Int -> Handle -> IO Int
    fputc c h = hPutChar h (chr c) >> return 0

    fgetc :: Handle -> IO Int
    fgetc h = handle (\ (_ :: SomeException) -> return (-1)) (do c <- hGetChar h; return (ord c))

    fopen :: Ptr CChar -> Ptr CChar -> IO Handle
    fopen name mode = do
      sname <- peekCAString name
      smode <- peekCAString mode
      let hmode =
            case smode of
              "r" -> ReadMode
              "w" -> WriteMode
              "a" -> AppendMode
              "w+" -> ReadWriteMode
              _ -> error "fopen"
      openFile sname hmode
