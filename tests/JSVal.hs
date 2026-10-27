module JSVal(main) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List(isInfixOf)
import Foreign.Ptr
import GHC.Wasm.Prim
import System.Mem(performGC)

foreign import javascript "$1 + $2"                     add        :: Int -> Int -> Int
foreign import javascript "return $1 * $2;"             mul        :: Double -> Double -> Double
foreign import javascript "$1 > 3"                      gt3        :: Int -> Bool
foreign import javascript "$1 ? 'yes' : 'no'"           boolStr    :: Bool -> JSString
foreign import javascript "String.fromCodePoint($1)"    charStr    :: Char -> JSString
foreign import javascript "$1.codePointAt(0)"           firstChar  :: JSString -> Char
foreign import javascript "({ a: $1, b: $2 })"          mkObj      :: Int -> JSString -> IO JSVal
foreign import javascript "$1[$2]"                      getProp    :: JSVal -> JSString -> IO JSVal
foreign import javascript "$1[$2] = $3"                 setProp    :: JSVal -> JSString -> JSVal -> IO ()
foreign import javascript "JSON.stringify($1)"          stringify  :: JSVal -> IO JSString
foreign import javascript "typeof $1"                   typeOf     :: JSVal -> IO JSString
foreign import javascript "$1"                          intToJSVal :: Int -> IO JSVal
foreign import javascript "$1"                          jsValToInt :: JSVal -> IO Int
foreign import javascript "$1"                          strToJSVal :: JSString -> IO JSVal
foreign import javascript "[1,2,3].map($1)"             mapJS      :: JSVal -> IO JSVal
foreign import javascript "$1()"                        call0      :: JSVal -> IO JSVal
foreign import javascript "$1($2, $3)"                  call2      :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript "Promise.resolve().then($1)"  later      :: JSVal -> IO ()
foreign import javascript "$1.then($2)"                 thenP      :: JSVal -> JSVal -> IO ()
foreign import javascript "mhsjs.kv.size"               tableSize  :: IO Int
foreign import javascript "globalThis"                  global     :: JSVal
foreign import javascript "typeof globalThis.mhsjs_free_test"
                                                        freeTest   :: IO JSString
foreign import javascript safe "$1.no.such"             badProp    :: JSVal -> IO JSVal
foreign import javascript interruptible "await $1"      await      :: JSVal -> IO JSVal
foreign import javascript interruptible "$1"            idPtr      :: Ptr a -> IO (Ptr a)
foreign import javascript "new Promise(function (r) { Promise.resolve().then(function () { r($1) }) })"
                                                        delayed    :: Int -> IO JSVal
foreign import javascript "Promise.reject(new Error('nope'))"
                                                        rejected   :: IO JSVal
-- Multi-line code with a comment
foreign import javascript "// a comment\n\
                          \ var s = 0;\n\
                          \ for (var i = 1; i <= $1; i++) s += i;\n\
                          \ return s; // done"
                                                        sumTo      :: Int -> Int

main :: IO ()
main = do
  print (add 3 4)
  print (mul 3 4)
  print (gt3 2, gt3 5)
  print (fromJSString (boolStr True), fromJSString (boolStr False))
  print (fromJSString (charStr 'x'), fromJSString (charStr '\955'))
  print (firstChar (toJSString "abc"), firstChar (toJSString "\955"))
  print (sumTo 100)

  o <- mkObj 42 (toJSString "hello, \955 world")
  stringify o >>= putStrLn . fromJSString
  a <- getProp o (toJSString "a")
  jsValToInt a >>= print
  typeOf a >>= print . fromJSString
  typeOf o >>= print . fromJSString
  n <- getProp o (toJSString "nothing")
  print (isUndefined n, isNull n, isUndefined a, isNull jsNull)
  setProp o (toJSString "b") jsNull
  stringify o >>= print . fromJSString
  print (fromJSString (toJSString "abc") == "abc", fromJSString (toJSString "abc") == "abd")
  print (textFromJSString (textToJSString "text \955"))
  print (textFromJSString (textToJSString "nul\0byte"))
  idPtr (nullPtr `plusPtr` 12) >>= \ p -> print (p == nullPtr `plusPtr` 12)

  -- synchronous callbacks
  dbl <- syncCallback1' $ \ v -> do
    i <- jsValToInt v
    intToJSVal (i * 2)
  mapJS dbl >>= stringify >>= print . fromJSString
  r <- newIORef (0::Int)
  cnt <- syncCallback2 $ \ x y -> do
    i <- jsValToInt x
    j <- jsValToInt y
    modifyIORef r (+ (i + j))
  vx <- intToJSVal 10
  vy <- intToJSVal 5
  replicateM_ 3 $ call2 cnt vx vy
  readIORef r >>= print
  -- a callback that calls back into JavaScript, which calls back into Haskell
  nest <- syncCallback' $ do
    v <- mapJS dbl
    stringify v >>= putStrLn . ("nested: " ++) . fromJSString
    strToJSVal (toJSString "nested done")
  call0 nest >>= stringify >>= print . fromJSString

  -- handles are dropped when JSVals are garbage collected
  s0 <- tableSize
  forM_ [1..1000::Int] $ \ i -> mkObj i (toJSString "x")
  performGC
  s1 <- tableSize
  putStrLn $ if s1 < s0 + 1000 then "GC frees handles" else "GC did not free handles " ++ show (s0, s1)
  freeJSVal o
  freeJSVal dbl

  -- JavaScript exceptions with safe imports
  r1 <- try (badProp n)
  case r1 of
    Left e@(JSException _) -> putStrLn $ "caught " ++ (if "TypeError" `isInfixOf` show e then "TypeError" else show e)
    Right _ -> putStrLn "no exception"
  -- the exception value survives a handler of the wrong type
  r1' <- try (try @IOException (badProp n))
  case r1' of
    Left e@(JSException _) -> putStrLn $ "rethrown " ++ (if "TypeError" `isInfixOf` show e then "TypeError" else show e)
    Right _ -> putStrLn "no exception"
  -- interruptible imports wait for a Promise
  v <- await =<< delayed 42
  jsValToInt v >>= print
  r2 <- try (await =<< rejected)
  case r2 of
    Left e@(JSException _) -> putStrLn $ "caught " ++ (if "nope" `isInfixOf` show e then "nope" else show e)
    Right _ -> putStrLn "no exception"

  -- asynchronous callbacks run after main has finished
  done <- syncCallback1 $ \ _ -> putStrLn "promise resolved"
  acb <- asyncCallback $ putStrLn "async callback called"
  later acb
  p <- call0 acb
  thenP p done
  -- threads keep running after main: a thread woken by a callback runs right
  -- after the callback, and a thread in threadDelay is resumed by a timer
  mv <- newEmptyMVar
  _ <- forkIO $ takeMVar mv >>= \ s -> putStrLn ("woken " ++ s)
  wake <- syncCallback $ putMVar mv "by callback"
  later wake
  _ <- forkIO $ threadDelay 20000 >> putStrLn "timer thread ran"
  putStrLn "main done"
