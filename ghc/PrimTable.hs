module PrimTable(module PrimTable) where
import Control.Exception
import Data.Bits
import Data.Char
import Data.Int
import Data.Maybe
import Data.Word
import System.IO
import System.IO.TimeMilli
import Unsafe.Coerce
import GHC.Exts(Any)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Data.Array.IO
import Foreign.Ptr
import GHC.ForeignPtr
--import System.Environment
import System.IO.Unsafe
--import Debug.Trace


type AnyType = Any

type IOArr a = IOArray Int a

_primitive :: String -> Any
--_primitive s | trace ("_primitive " ++ show s) False = undefined
_primitive "dynsym" = unsafeCoerce dynsym
_primitive s = fromMaybe (error $ "PrimTable._primitive: " ++ s) $ lookup s primOps

primOps :: [(String, Any)]
primOps =
  [ comb "S" (\ f g x -> f x (g x))
  , comb "K" const
  , comb "I" id
  , comb "B" (\ f g x -> f (g x))
  , comb "C" (\ f g x -> f x g)
  , comb "S'" (\ k f g x -> k (f x) (g x))
  , comb "B'" (\ k f g x -> k f (g x))
  , comb "C'" (\ k f g x -> k (f x) g)
  , comb "A" (\ _x y -> y)
  , comb "U" (\ x y -> y x)
  , comb "Y" (\ f -> let r = f r in r)
  , comb "Z" (\ f g _x -> f g)
  , comb "J" (\ x _y z -> z x)
  , comb "P" (\ x y f -> f x y)
  , comb "R" (\ x y f -> y f x)
  , comb "O" (\ x y _g f -> f x y)
  , comb "K2" (\ x _y _z -> x)
  , comb "K3" (\ x _y _z _w -> x)
  , comb "K4" (\ x _y _z _w _v -> x)
  , comb "C'B" (\ x y z w -> x z (y w))

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
  , arithu "popcount" popCount
  , arithu "clz" countLeadingZeros
  , arithu "ctz" countTrailingZeros
  , cmp "==" (==)
  , cmp "/=" (/=)
  , cmp "<"  (<)
  , cmp "<=" (<=)
  , cmp ">"  (>)
  , cmp ">=" (>=)
  , cmpw "u<"  (<)
  , cmpw "u<=" (<=)
  , cmpw "u>"  (>)
  , cmpw "u>=" (>=)
  , comb "icmp" (\ x y -> fromOrdering (compare (x::Int) y))
  , comb "ucmp" (\ x y -> fromOrdering (compare (x::Word) y))

  , arithI "I+" (+)
  , arithI "I-" (-)
  , arithI "I*" (*)
  , arithI "Iquot" quot
  , arithI "Irem" rem
  , arithI "Isubtract" subtract
  , arithuI "Ineg" negate
  , arithuI "Iinv" complement
  , arithwI "Iuquot" quot
  , arithwI "Iurem" rem
  , arithwI "Iand" (.&.)
  , arithwI "Ior" (.|.)
  , arithwI "Ixor" xor
  , arithwiI "Ishl" shiftL
  , arithwiI "Ishr" shiftR
  , arithwiI "Iashr" shiftR
  , arithuiI "Ipopcount" popCount
  , arithuiI "Iclz" countLeadingZeros
  , arithuiI "Ictz" countTrailingZeros
  , cmpI "I==" (==)
  , cmpI "I/=" (/=)
  , cmpI "I<"  (<)
  , cmpI "I<=" (<=)
  , cmpI "I>"  (>)
  , cmpI "I>=" (>=)
  , cmpwI "Iu<"  (<)
  , cmpwI "Iu<=" (<=)
  , cmpwI "Iu>"  (>)
  , cmpwI "Iu>=" (>=)
  , comb "Iicmp" (\ x y -> fromOrdering (compare (x::Int64) y))
  , comb "Iucmp" (\ x y -> fromOrdering (compare (x::Word64) y))

  , comb "itoI" (\ x -> fromIntegral (x::Int) :: Int64)
  , comb "Itoi" (\ x -> fromIntegral (x::Int64) :: Int)
  , comb "utoU" (\ x -> fromIntegral (x::Word) :: Word64)
  , comb "Utou" (\ x -> fromIntegral (x::Word64) :: Word)

  , comb "fp2p" unsafeForeignPtrToPtr

  , comb "A.alloc" newIOArray
  , comb "A.size" sizeIOArray
  , comb "A.read" readIOArray
  , comb "A.write" writeIOArray
--  , comb "A.==" eqIOArray

  , farith "f+" (+)
  , farith "f-" (-)
  , farith "f*" (*)
  , farith "f/" (/)
  , farithu "fneg" negate
  , fcmp "f==" (==)
  , fcmp "f/=" (/=)
  , fcmp "f<" (<)
  , fcmp "f<=" (<=)
  , fcmp "f>" (>)
  , fcmp "f>=" (>=)
  , comb "itof" (fromIntegral :: Int -> Float)

  , darith "d+" (+)
  , darith "d-" (-)
  , darith "d*" (*)
  , darith "d/" (/)
  , darithu "dneg" negate
  , dcmp "d==" (==)
  , dcmp "d/=" (/=)
  , dcmp "d<" (<)
  , dcmp "d<=" (<=)
  , dcmp "d>" (>)
  , dcmp "d>=" (>=)
  , comb "itod" (fromIntegral :: Int -> Double)

  , comb "seq" seq
  , comb "rnf" rnf
  , comb "ord" ord
  , comb "chr" chr

  , comb "IO.performIO" unsafePerformIO
  , comb "IO.>>=" iobind
  , comb "IO.>>" iothen
  , comb "IO.return" ioret
  , comb "IO.print" ioprint
  , comb "IO.serialize" ioserialize
  , comb "IO.deserialize" iodeserialize
  , comb "newCAStringLen" (fmap fromPair . newCAStringLen . toString)
  , comb "IO.getArgRef" iogetargref

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

    arithI :: String -> (Int64 -> Int64 -> Int64) -> (String, Any)
    arithI = comb
    arithwI :: String -> (Word64 -> Word64 -> Word64) -> (String, Any)
    arithwI = comb
    arithwiI :: String -> (Word64 -> Int -> Word64) -> (String, Any)
    arithwiI = comb
    arithuI :: String -> (Int64 -> Int64) -> (String, Any)
    arithuI = comb
    arithuiI :: String -> (Int64 -> Int) -> (String, Any)
    arithuiI = comb

    farith :: String -> (Float -> Float -> Float) -> (String, Any)
    farith = comb
    farithu :: String -> (Float -> Float) -> (String, Any)
    farithu = comb

    darith :: String -> (Double -> Double -> Double) -> (String, Any)
    darith = comb
    darithu :: String -> (Double -> Double) -> (String, Any)
    darithu = comb

    cmp :: String -> (Int -> Int -> Bool) -> (String, Any)
    cmp n f = comb n (\ x y -> fromBool (f x y))
    cmpw :: String -> (Word -> Word -> Bool) -> (String, Any)
    cmpw n f = comb n (\ x y -> fromBool (f x y))

    cmpI :: String -> (Int64 -> Int64 -> Bool) -> (String, Any)
    cmpI n f = comb n (\ x y -> fromBool (f x y))
    cmpwI :: String -> (Word64 -> Word64 -> Bool) -> (String, Any)
    cmpwI n f = comb n (\ x y -> fromBool (f x y))

    fcmp :: String -> (Float -> Float -> Bool) -> (String, Any)
    fcmp n f = comb n (\ x y -> fromBool (f x y))
    dcmp :: String -> (Double -> Double -> Bool) -> (String, Any)
    dcmp n f = comb n (\ x y -> fromBool (f x y))

    iobind :: IO a -> (a -> IO b) -> IO b
    iobind = (>>=)
    iothen :: IO a -> IO b -> IO b
    iothen = (>>)
    ioret :: a -> IO a
    ioret = return
    -- Can't implement this
    ioprint :: Handle -> a -> IO ()
    ioprint h _ = hPutStrLn h "ghc does not support cprint"
    ioserialize :: Handle -> a -> IO ()
    ioserialize h _ = hPutStrLn h "ghc does not support serialize"
    iodeserialize :: Handle -> IO a
    iodeserialize _ = error "ghc does not support deserialize"

{-
    iogetargs :: IO Any
    iogetargs = do
      args <- getArgs
      return $ fromList $ map fromString args
-}
    iogetargref = error "ghc: no IO.getArgRef"

    -- Can't implement this
    rnf :: a -> ()
    rnf x = seq x ()

    newIOArray :: Int -> a -> IO (IOArr a)
    newIOArray i arr = newArray (0, i-1) arr
    sizeIOArray :: IOArr a -> IO Int
    sizeIOArray arr = pred . snd <$> getBounds arr
    readIOArray :: IOArr a -> Int -> IO a
    readIOArray arr i = readArray arr i
    writeIOArray :: IOArr a -> Int -> a -> IO ()
    writeIOArray arr i a = writeArray arr i a
--    eqIOArray :: IOArr a -> IOArr a -> IO Bool


fromBool :: Bool -> Any
fromBool False = unsafeCoerce const
fromBool True  = unsafeCoerce $ \ _x y -> y

fromOrdering :: Ordering -> (Any -> Any -> Any -> Any)
fromOrdering LT = \ x _y _z -> x
fromOrdering EQ = \ _x y _z -> y
fromOrdering GT = \ _x _y z -> z

fromPair :: (a, b) -> Any
fromPair (x, y) = unsafeCoerce $ \ pair -> pair x y

fromString :: String -> Any
fromString = fromList . map (unsafeCoerce . ord)

fromList :: [Any] -> Any
fromList [] = unsafeCoerce const
fromList (x:xs) = unsafeCoerce $ \ _nil cons -> cons (unsafeCoerce x) (fromList xs)

toList :: Any -> [Int]
toList a = unsafeCoerce a [] (\ i is -> i : toList is)

toString :: Any -> String
toString = map chr . toList

dynsym :: Any -> Any
dynsym acfun =
  let s = toString acfun
  in
--      trace ("dynsym: " ++ show s) $
      fromMaybe (error $ "ghc: unimplemented FFI: " ++ s) $ lookup s cops

cops :: [(String, Any)]
cops =
  [ comb "getTimeMilli" getTimeMilli
  , comb "fputc" fputc
  , comb "fgetc" fgetc
  , comb "fopen" fopen
  , comb "fclose" fclose
  , comb "putb"  putb
  , comb "add_FILE" add_FILE
  , comb "add_utf8" add_utf8
  , comb "free"  free
  , comb "exp"   (dio exp)
  , comb "log"   (dio log)
  , comb "sqrt"   (dio sqrt)
  , comb "sin"   (dio sin)
  , comb "cos"   (dio cos)
  , comb "tan"   (dio tan)
  , comb "asin"   (dio asin)
  , comb "acos"   (dio acos)
  , comb "atan"   (dio atan)
  , comb "sinh"   (dio sinh)
  , comb "cosh"   (dio cosh)
  , comb "tanh"   (dio tanh)
  , comb "asinh"   (dio asinh)
  , comb "acosh"   (dio acosh)
  , comb "atanh"   (dio atanh)
  , comb "atan2"   (dio2 atan2)
  , comb "expf"   (fio exp)
  , comb "logf"   (fio log)
  , comb "sqrtf"   (fio sqrt)
  , comb "sinf"   (fio sin)
  , comb "cosf"   (fio cos)
  , comb "tanf"   (fio tan)
  , comb "asinf"   (fio asin)
  , comb "acosf"   (fio acos)
  , comb "atanf"   (fio atan)
  , comb "sinhf"   (fio sinh)
  , comb "coshf"   (fio cosh)
  , comb "tanhf"   (fio tanh)
  , comb "asinhf"   (fio asinh)
  , comb "acoshf"   (fio acosh)
  , comb "atanhf"   (fio atanh)
  , comb "atan2f"   (fio2 atan2)
  ]
  where
    comb n f = (n, unsafeCoerce f)

    dio :: (Double -> Double) -> (Double -> IO Double)
    dio f = return . f

    dio2 :: (Double -> Double -> Double) -> (Double -> Double -> IO Double)
    dio2 f x y = return (f x y)

    fio :: (Float -> Float) -> (Float -> IO Float)
    fio f = return . f

    fio2 :: (Float -> Float -> Float) -> (Float -> Float -> IO Float)
    fio2 f x y = return (f x y)

    add_FILE :: Handle -> IO Handle
    add_FILE h = return h

    add_utf8 :: Handle -> IO Handle
    add_utf8 h = do hSetEncoding h utf8; return h

    putb :: Int -> Handle -> IO ()
    putb c h = hPutChar h (chr c)

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

    fclose :: Handle -> IO Int
    fclose h = do hClose h; return 0
