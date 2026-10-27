-- Callbacks from JavaScript into Haskell, exceptions and threads (emscripten target).
module JSCallback(main) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List(isInfixOf)
import Data.Maybe(fromMaybe)
import GHC.Wasm.Prim
import System.Mem (performGC)

-- Call the callback from the JavaScript event loop (after ms milliseconds).
foreign import javascript "setTimeout(() => $1(), $2)" later :: JSVal -> Int -> IO ()
-- Call the callback from a JavaScript timer, and report an exception thrown by it.
foreign import javascript "setTimeout(() => { try { $1() } catch (e) { console.log('JavaScript caught: ' + e.message) } }, $2)"
  laterCatch :: JSVal -> Int -> IO ()
-- Call an asynchronous callback (returns a Promise) and report a rejection.
foreign import javascript "$1().then(() => console.log('promise resolved'), e => console.log('promise rejected: ' + e.message))"
  callAsync :: JSVal -> IO ()
-- A Promise that resolves after ms milliseconds.
foreign import javascript "new Promise(r => setTimeout(() => r(42), $1))" delayed :: Int -> IO JSVal
foreign import javascript interruptible "return await $1" await :: JSVal -> IO JSVal
foreign import javascript "$1" jsValToInt :: JSVal -> IO Int
foreign import javascript "({ n: $1 })" mkObj :: Int -> IO JSVal
foreign import javascript safe "$1.n" getN :: JSVal -> IO Int
foreign import javascript "mhsjs.kv.size" tableSize :: IO Int
-- Call a callback that returns a value, and report what it returned.
foreign import javascript "return $1()" call0 :: JSVal -> IO JSVal
foreign import javascript "$1" intToJSVal :: Int -> IO JSVal
-- Keep a callback on the JavaScript side, and call it later.
foreign import javascript "globalThis.savedCb = $1" saveCb :: JSVal -> IO ()
foreign import javascript safe "globalThis.savedCb()" callSavedCb :: IO ()
-- The same code in a synchronous and an interruptible import.
foreign import javascript safe "return $1 + 1" plus1 :: Int -> IO Int
foreign import javascript interruptible "return $1 + 1" plus1Async :: Int -> IO Int
foreign import javascript safe "throw new Error('callback boom')" jsThrow :: IO ()
foreign import javascript "throw new Error('unsafe boom')" jsThrowUnsafe :: IO ()
-- Call the callback n times, ignoring the errors it throws.
foreign import javascript "for (var i = 0; i < $2; i++) { try { $1() } catch (e) {} }" callMany :: JSVal -> Int -> IO ()
-- Call the callback now, and report an exception thrown by it.
foreign import javascript "try { $1() } catch (e) { console.log('JavaScript caught: ' + e.message) }" callCatch :: JSVal -> IO ()

-- An exception that cannot be shown.
data BadShow = BadShow
instance Show BadShow where
  show _ = error "show failed"
instance Exception BadShow

-- Call the value callback $1 now, and report whether its result arrived or it threw.
foreign import javascript "try { var r = $1(); console.log('callback 19b: ' + (r && r.n === 19 ? 'result arrived' : 'result lost')) } catch (e) { console.log('callback 19b threw: ' + e.message) }"
  callValueCatch :: JSVal -> IO ()

-- From the event loop: call the value callback $1, and report to $2 whether its result arrived.
foreign import javascript "setTimeout(() => { var ok; try { ok = $1().n === 13 ? 1 : 0; } catch (e) { console.log('ERROR: ' + e.message); ok = 0; } $2(ok); }, 10)"
  laterValue :: JSVal -> JSVal -> IO ()

main :: IO ()
main = do
  mainTid <- myThreadId
  mv <- newEmptyMVar

  -- 1. A callback runs as a thread of its own while main is blocked in takeMVar:
  --    catch, masking state, myThreadId and forkIO work.
  cb1 <- syncCallback $ do
    r <- try (evaluate (1 `div` (0 :: Int)))
    putStrLn $ "callback: " ++ either (\ e -> "caught " ++ show (e :: ArithException)) (const "no exception") r
    st <- getMaskingState
    putStrLn $ "callback: masking state " ++ show st
    tid <- myThreadId
    putStrLn $ "callback: own thread " ++ show (tid /= mainTid)
    _ <- forkIO $ putStrLn "forked thread ran"
    putMVar mv "callback 1"
  later cb1 10
  putStrLn "main: waiting"
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 2. An uncaught exception in a callback becomes a JavaScript Error; the runtime continues.
  cb2 <- syncCallback $ do
    putStrLn "callback: throwing"
    _ <- throwIO (ErrorCall "boom")
    putMVar mv "unreachable"
  cb3 <- syncCallback $ putMVar mv "callback 3"
  laterCatch cb2 10
  later cb3 20
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 3. An asynchronous callback that throws rejects its Promise.
  acb <- asyncCallback $ throwIO (ErrorCall "async boom")
  callAsync acb
  acb2 <- asyncCallback $ putMVar mv "callback 4"
  callAsync acb2
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 4. killThread from a callback reaches a thread sleeping in threadDelay while main is blocked.
  t <- forkIO $ (threadDelay 5000000 >> putStrLn "not killed")
                  `catch` \ e -> putStrLn ("thread got: " ++ show (e :: AsyncException))
  cb4 <- syncCallback $ killThread t >> putMVar mv "callback 5"
  later cb4 10
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 5. A callback runs while main awaits a Promise (interruptible import), and the
  --    callback can await one too: it blocks, and continues from the event loop.
  cb5 <- syncCallback $ do
    putStrLn "callback: during await"
    r <- try (delayed 1 >>= await)
    case r of
      Left e -> putStrLn $ "callback: nested await failed: " ++ show (e :: JSException)
      Right v5 -> jsValToInt v5 >>= \ n -> putStrLn ("callback: nested await done " ++ show n)
  later cb5 10
  v <- await =<< delayed 50
  jsValToInt v >>= \ n -> putStrLn ("main: await done " ++ show n)

  -- 6. Freeing a JSVal twice is harmless; using it afterwards is a JSException with safe.
  o <- mkObj 7
  freeJSVal o
  freeJSVal o
  r <- try (getN o)
  putStrLn $ either (\ e -> if "freed" `isInfixOf` show (e :: JSException) then "use after free: caught" else show e) (const "use after free: no exception?") r

  -- 7. Many short-lived JSVals do not grow the handle table without bound
  --    (the GC is forced after JSVAL_GC_LIMIT new handles).
  s0 <- tableSize
  forM_ [1 .. 150000 :: Int] $ \ i -> mkObj i >>= getN >>= \ n -> when (n /= i) (putStrLn "bad n")
  s1 <- tableSize
  putStrLn $ if s1 - s0 < 150000 then "handle table bounded" else "handle table grew by " ++ show (s1 - s0)

  -- 8. A callback that returns a JSVal runs its action exactly once.
  cnt <- newIORef (0 :: Int)
  cbr <- syncCallback' $ do
    modifyIORef cnt (+ 1)
    n <- readIORef cnt
    intToJSVal n
  v8 <- call0 cbr
  n8 <- jsValToInt v8
  runs <- readIORef cnt
  putStrLn $ "value callback: returned " ++ show n8 ++ ", ran " ++ show runs ++ " time(s)"

  -- 9. Calling a callback after freeJSVal is an error; it must not run whichever
  --    callback has been given the freed stable pointer since.
  hit <- newIORef "no callback"
  cbA <- syncCallback $ writeIORef hit "callback A"
  saveCb cbA
  freeJSVal cbA
  cbB <- syncCallback $ writeIORef hit "callback B"
  r9 <- try callSavedCb
  ran <- readIORef hit
  putStrLn $ "freed callback: " ++
    either (\ e -> if "after being freed" `isInfixOf` show (e :: JSException) then "caught" else show e) (const "no exception") r9 ++
    ", " ++ ran ++ " ran"
  freeJSVal cbB

  -- 10. The same code in a synchronous and an interruptible import: each gets its own
  --     compiled function (the synchronous one must not get a Promise).
  a10 <- plus1Async 1
  s10 <- plus1 1
  putStrLn $ "same code, sync and async: " ++ show (a10, s10)

  -- 11. A callback dying of an uncaught JSException does not leak the exception's
  --     value: it is an ordinary JSVal, which the GC frees with the exception.
  cb11 <- syncCallback jsThrow
  performGC
  s2 <- tableSize
  callMany cb11 50
  performGC
  s3 <- tableSize
  putStrLn $ if s3 - s2 < 5 then "dying callbacks: no leak" else "dying callbacks: leaked " ++ show (s3 - s2)

  -- 12. throwTo from a callback to a thread that already has an exception pending
  --     must not block (the target is uninterruptible, so it takes neither yet).
  r12 <- newEmptyMVar
  t12 <- forkIO $ uninterruptibleMask_ (threadDelay 30000) >> putMVar r12 "not killed"
  cb12 <- syncCallback $ do
    throwTo t12 (ErrorCall "one")
    throwTo t12 (ErrorCall "two")
    putMVar mv "callback 12"
  later cb12 5
  takeMVar mv >>= putStrLn . ("main: got " ++)
  threadDelay 60000
  tryTakeMVar r12 >>= \ m -> putStrLn $ "throwTo twice from a callback: " ++ fromMaybe "target killed" m

  -- 14. A callback dying of an exception whose show raises another one: the
  --     calling thread's stack must be left as it was.
  cb14 <- syncCallback $ throwIO BadShow
  xs <- mapM (\ i -> callCatch cb14 >> return (i * i)) [1 .. 5 :: Int]
  putStrLn $ "unshowable exception in a callback: " ++ show (sum xs)

  -- 17. A callback called from the event loop may block: it waits like any other
  --     thread while the others (here main) run, and continues when it is woken.
  gate <- newEmptyMVar
  cb17 <- syncCallback $ do
    putStrLn "callback: blocking"
    takeMVar gate
    putStrLn "callback: unblocked"
    putMVar mv "callback 17"
  later cb17 5
  threadDelay 20000
  putStrLn "main: opening the gate"
  putMVar gate ()
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 18. A callback that was left blocked and then dies of an uncaught exception:
  --     there is no JavaScript caller to throw it to, so it is reported (stderr).
  gate18 <- newEmptyMVar
  cb18 <- syncCallback $ takeMVar gate18 >> throwIO (ErrorCall "late boom")
  later cb18 5
  threadDelay 20000
  putMVar gate18 ()
  threadDelay 20000

  -- 13. A value callback called from the JavaScript event loop, which wakes a thread
  --     that forces a GC: the result must reach JavaScript before that thread runs.
  gcgo <- newEmptyMVar
  _ <- forkIO $ forever $ do
    takeMVar gcgo
    replicateM_ 100001 (mkObj 0)
  cb13 <- syncCallback' $ do
    putMVar gcgo ()
    mkObj 13
  report <- syncCallback1 $ \ ok -> do
    n <- jsValToInt ok
    putStrLn $ "value callback from the event loop: " ++ (if n == 1 then "result arrived" else "result lost")
  laterValue cb13 report

  -- 15. yield in a callback does nothing (other threads exist, but a callback
  --     cannot switch to them).
  cb15 <- syncCallback $ yield >> putMVar mv "callback 15"
  later cb15 5
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 19. A callback from the event loop wakes a thread and then dies of an exception.
  --     The thread runs before that callback returns to JavaScript, and calls a
  --     second callback from an import: that one must neither throw the first one's
  --     exception nor lose its result, and the first one's caller must get it.
  gate19 <- newEmptyMVar
  done19 <- newEmptyMVar
  cb19b <- syncCallback' $ mkObj 19
  _ <- forkIO $ do
    takeMVar gate19
    callValueCatch cb19b
    putMVar done19 ()
  cb19 <- syncCallback $ putMVar gate19 () >> throwIO (ErrorCall "boom 19")
  laterCatch cb19 50   -- after test 13's timer, so the output order is fixed
  takeMVar done19
  threadDelay 20000

  -- 16. A JavaScript exception from an unsafe import in a callback unwinds the C
  --     stack of the runtime (fatal): later callbacks are refused with an error
  --     instead of running on the wreckage.
  cb16 <- syncCallback jsThrowUnsafe
  cb16' <- syncCallback $ putStrLn "callback 16' ran?"
  laterCatch cb16 30
  laterCatch cb16' 40

  putStrLn "main: done"
