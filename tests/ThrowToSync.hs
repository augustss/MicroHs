module ThrowToSync where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

-- throwTo does not return until the exception has been raised in the target
-- thread, as in GHC.  Each check below depends on that.

main :: IO ()
main = do
  -- The target is runnable rather than blocked: it has been forked but has not
  -- had a turn yet.  killThread must still not return until it has died.
  r1 <- newIORef "not raised"
  t1 <- forkIO $ forever (threadDelay 1000)
        `catch` \ e -> writeIORef r1 (show (e :: SomeException))
  killThread t1
  readIORef r1 >>= \ s -> putStrLn ("runnable target: " ++ s)

  -- The target is blocked on an MVar.
  m2 <- newEmptyMVar :: IO (MVar ())
  r2 <- newIORef "not raised"
  t2 <- forkIO $ takeMVar m2
        `catch` \ e -> writeIORef r2 (show (e :: SomeException))
  threadDelay 5000
  killThread t2
  readIORef r2 >>= \ s -> putStrLn ("blocked target: " ++ s)

  -- A doomed thread must not consume shared state after the kill.  Both workers
  -- take from one MVar; the first is killed before it ever runs, so the value
  -- has to go to the second.
  q <- newEmptyMVar :: IO (MVar ())
  who <- newIORef "nobody"
  old <- forkIO $ forever (takeMVar q >> writeIORef who "the killed thread")
  killThread old
  _ <- forkIO $ forever (takeMVar q >> writeIORef who "the live thread")
  putMVar q ()
  threadDelay 20000
  readIORef who >>= \ w -> putStrLn ("the value went to " ++ w)

  -- Killing an already dead thread is a no-op, and must not block.
  killThread old
  killThread old
  putStrLn "killing a dead thread twice is a no-op"

  -- throwTo to ourselves must not deadlock: the exception is raised at the next
  -- interruption point, not inside throwTo.
  me <- myThreadId
  r3 <- try (throwTo me (userError "self") >> threadDelay 1000)
  putStrLn ("self throwTo: " ++ either (\ e -> show (e :: SomeException)) (const "no exception") r3)

  -- An uninterruptible target is not interrupted, so the kill is deferred until
  -- it leaves the mask; killThread waits that long.
  m4 <- newEmptyMVar :: IO (MVar ())
  r4 <- newIORef "not raised"
  t4 <- forkIO $ (uninterruptibleMask_ (threadDelay 30000) >> takeMVar m4)
        `catch` \ e -> writeIORef r4 (show (e :: SomeException))
  threadDelay 5000
  killThread t4
  readIORef r4 >>= \ s -> putStrLn ("masked target: " ++ s)

  -- Two threads kill a target that finishes without ever being interruptible.
  -- The first exception waits in the target's exception slot, the second thread
  -- waits because the slot is full.  Both killThreads return when the target ends.
  m5 <- newEmptyMVar :: IO (MVar ())
  t5 <- forkIO $ uninterruptibleMask_ (threadDelay 30000)
  threadDelay 5000
  _ <- forkIO $ killThread t5 >> putMVar m5 ()
  _ <- forkIO $ killThread t5 >> putMVar m5 ()
  takeMVar m5
  takeMVar m5
  putStrLn "two killThreads of a finishing target both return"

  -- A throwTo that is interrupted has no effect.  Thread b is masked, so the
  -- throwTo from thread a waits; a is killed while it waits, and b must then
  -- not receive a's exception.
  r6 <- newIORef "not set"
  d6 <- newEmptyMVar :: IO (MVar ())
  b6 <- forkIO $ ((uninterruptibleMask_ (threadDelay 30000) >> threadDelay 10000 >> writeIORef r6 "finished normally")
                   `catch` \ e -> writeIORef r6 ("received " ++ show (e :: ErrorCall)))
                  `finally` putMVar d6 ()
  threadDelay 5000
  a6 <- forkIO $ throwTo b6 (ErrorCall "from a")
  threadDelay 5000
  killThread a6
  takeMVar d6
  readIORef r6 >>= \ s -> putStrLn ("interrupted throwTo: target " ++ s)
