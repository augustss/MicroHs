module Main where
import Data.List
--import System.Random
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Environment
-- import Control.Concurrent.Future

main = do 
  l <- getArgs
  if length l < 2 then do
           putStrLn "Please choose between different tests"
           putStrLn "====================================="
           putStrLn "(1) multiple threads increase the value of a single TVar"
           putStrLn "(2) dining philosophers"
           putStrLn "(3) computation on a nxn matrix with nxn threads"
           putStrLn "(4) Example: MVars encoded by STM"
           f <- readLn -- catch (readLn) (\_ -> main)
           case f of
             1 -> mainSingleTVar []
             2 -> diningPhilosophers []
             3 -> mainMatrix []
             4 -> mainMVar []
             _ -> main
   else do
    let (a:b) = l
    let x = read a
    case x of
             1 -> mainSingleTVar b
             2 -> diningPhilosophers b
             3 -> mainMatrix b
             4 -> mainMVar b
             _ -> main
    

mainSingleTVar i = do
  num <- do
   if null i then do
     putStrLn "This example uses a single TVar with content 0 at the beginning"
     putStrLn "Then every thread performs the transaction of increasing the TVar by one"
     putStrLn "Finally, the content of the the TVar is printed by some further thread"
     putStrLn "=================================================="
     putStrLn "How many threads should run?"
     readLn
    else return (read $ head i)
  x <- newTVarIO 0
  mvars <- sequence $ replicate num (newEmptyMVar)
  sequence_ [forkIO (increaseTVar x >> putMVar y ()) | y <- mvars]
  sequence_ [takeMVar y | y <- mvars]
  z <- atomically (readTVar x)
  putStrLn $ "The final result is " ++ show z
 where  
  increaseTVar x = 
   atomically (readTVar x >>= (writeTVar x) . (+1))

diningPhilosophers i = do
  n <- do
   if null i then do
     putStrLn "=================================================="
     putStrLn "How many philosophers?"
     readLn
    else return (read $ head i)
  maxrounds <- do
   if null i || null (tail i)  then do
     putStrLn "=================================================="
     putStrLn "How many rounds?"
     readLn
    else return (read $ head $ tail i)
  forks <- atomically $ sequence (replicate n (newTVar 0))
  mv <- newEmptyMVar
  sequence_ [forkIO (philosopher maxrounds n i forks >> putMVar mv ()) | i <- [0..(n-1)]]
  sequence_ [takeMVar mv | i <- [0..(n-1)]]
  putStrLn "done"

     
philosopher 0 n i forks = return ()
philosopher r n i forks = do
  getForks (forks!!i) (forks!!((i+1) `mod` n))
--  sPutStrLn $ "philosopher " ++ show i ++ " eats ..."
  releaseForks (forks!!i) (forks!!((i+1) `mod` n))
--  sPutStrLn $ "philosopher " ++ show i ++ " thinks ..."
  someDelay
  philosopher (r-1) n i forks  
     
     
getForks left right = atomically $
 do
  l <- readTVar left
  if l == 1 then retry
   else do
         writeTVar left 1
         r <- readTVar right 
         if r == 1 then retry
          else writeTVar right 1
          
releaseForks left right = atomically $
 do
  writeTVar left 0
  writeTVar right 0

mainMatrix i =
 do
  putStrLn "==========================================================="
  putStrLn "This example starts with a nxn matrix of the form"
  putStrLn " 0  1  2  ...  n-1"
  putStrLn " n  n+1   ... 2n-1"
  putStrLn " .. .. ..      .. "
  putStrLn "every entry in the matrix is represented by a TVar"
  putStrLn "There are nxn threads for every entry one"
  putStrLn "Each thread reads the values 'around' its cell and"
  putStrLn "writes the maximal read value into its cell"
  n <- do
   if null i then do
     putStrLn "=================================================="
     putStrLn "Choose n?"
     readLn
    else return (read $ head i)
  matrix n  
  
  
  
matrix n =
 do 
  m <- atomically $ sequence [(sequence [newTVar (fromIntegral i) | i <- [w*n+0..w*n+n-1]]) | w <- [0..n-1]]
  zs <- atomically $ mapM (\l -> sequence [readTVar (l!!i)  | i <- [0..n-1]]) m
  putStrLn "Matrix before:"
  putStrLn $  unlines (map (\line -> (concat $ intersperse "\t" $ map show line)) zs)
  ack <- replicateM n $ sequence [newEmptyMVar | i <- [0..n-1]]
  sequence [forkIO (someDelay >> upD i j m >> putMVar (ack!!i!!j) ()) | i <- [0..n-1], j <- [0..n-1]]
  sequence_ [takeMVar a | a <- (concat ack)]
  zs <- atomically $ mapM (\l -> sequence [readTVar (l!!i)  | i <- [0..n-1]]) m
  putStrLn "Matrix after:"
  putStrLn $  unlines (map (\line -> (concat $ intersperse "\t" $ map show line)) zs)
 where
   upD i j m  = atomically $
    let n = genericLength m in
     do
       xs <- sequence [readTVar (m!!k!!l) | k <- [i-1,i,i+1], l <- [j-1,j,j+1], k >= 0, l >= 0, k < n, l < n, (i,j) /= (k,l)]
       writeTVar (m!!i!!j) (maximum xs)
          
         
  
    

  
  




someDelay =
   do
    i <- randomRIO (1,15)::IO Integer
    mid <- myThreadId
    threadDelay (2^i)

mainMVar i = 
   do
     putStrLn "This example uses MVars encoded by STM."
     putStrLn "An MVar is represented as a pair (a,b) of TVars (with Integers as content)"
     putStrLn "If a == 1 then the MVar is filled with b, otherwise the MVar is empty"
     putStrLn "The operations newMVar, takeMVar, putMVar are encoded as STM-transactions:"
     putStrLn ""
     putStrLn "   newMVar x = atomically $"
     putStrLn "     do"
     putStrLn "     filled  <- newTVar 1  "
     putStrLn "     content <- newTVar x "
     putStrLn "     return (filled,content)"
     putStrLn ""
     putStrLn "     takeMVar (filled,content) = atomically $"
     putStrLn "       do"
     putStrLn "       full  <- readTVar filled"
     putStrLn "       if full==1 then"
     putStrLn "         do"
     putStrLn "           writeTVar filled 0"
     putStrLn "           readTVar content"
     putStrLn "         else"
     putStrLn "           retry"
     putStrLn ""
     putStrLn "     putMVar (filled,content) newcontent = atomically $"
     putStrLn "       do"
     putStrLn "         full <- readTVar filled"
     putStrLn "         if full==1 then retry"
     putStrLn "         else "
     putStrLn "           do"
     putStrLn "             writeTVar filled 1"
     putStrLn "             writeTVar content newcontent"
     putStrLn ""
     putStrLn "The example now uses a single MVar to protect printing on stdout"
     putStrLn "And several threads want to print a message."
     n <- do
      if null i then do
        putStrLn "Choose the number of threads!"
        readLn
       else return (read $ head i)
     mainMyMVar n

type MyMVar = (TVar Integer,TVar Integer)   
   
newMyMVar x = atomically $
  do
   filled  <- newTVar 1  
   content <- newTVar x 
   return (filled,content)
   
takeMyMVar (filled,content) = atomically $
  do
   full  <- readTVar filled
   if full==1 then
     do
       writeTVar filled 0
       readTVar content
    else
      retry
      
putMyMVar (filled,content) newcontent = atomically $
  do
    full <- readTVar filled
    if full==1 then retry
     else 
      do
        writeTVar filled 1
        writeTVar content newcontent
        
        
        
testMyMVar mvar = 
  do
    mid <- myThreadId
    takeMyMVar mvar
    putStrLn ("Now printing with exclusive access: " ++ show mid)
    i <- randomRIO (1,80)
    putStrLn ("printing '.' " ++ show i ++ " times:")
    putStrLn (replicate i '.')
    putStrLn ("Now " ++ show mid ++ " finishes printing and leaves the critical section")
    putMyMVar mvar 0
    
mainMyMVar n = 
  do
   x <- newMyMVar 0
   xs <- sequence [newEmptyMVar | i <- [1..n]]
   sequence_ [forkIO ( do
                        res <- ((testMyMVar x))
                        mid <- myThreadId 
                        putMVar i res) | i <- xs]
   sequence_ [takeMVar i | i <- xs]



------------   
      
      
    
testOrElse x y =
    do
     orElse (do
              c <-readTVar x
              if c >= 1 then
                writeTVar x (c-1)
               else retry
             )
             (do
               c <-readTVar y
               if c >= 1 then
                 writeTVar y (c-1)
                else retry) 
     c1 <- readTVar x
     c2 <- readTVar y
     return (c1,c2)

              
mainOrElse = 
  do
   x <- newTVarIO 20
   y <- newTVarIO 20
   xs <- sequence [newEmptyMVar | i <- [1..40]]
   sequence_ [forkIO ( do
                        res <- (atomically (testOrElse x y))
                        mid <- myThreadId 
                        putMVar i res) | i <- xs]
   sequence_ [takeMVar i | i <- xs]

   
orElseNestedTest x1 x2 x3 x4 =
  orElse
   (orElse 
     (do 
        c <- readTVar x1
        writeTVar x1 (c+1)
        if (c+1) > 10 then retry else return ()) 
     (do 
        c <- readTVar x2
        writeTVar x2 (c+1)
        if (c+1) > 10 then retry else return ()) 

   )(orElse 
     (do 
        c <- readTVar x3
        writeTVar x3 (c+1)
        if (c+1) > 10 then retry else return ()) 
     (do 
        c <- readTVar x4
        writeTVar x4 (c+1)
     ))

       
      
nestedMain i =
  do
   x1 <- newTVarIO 0
   x2 <- newTVarIO 0
   x3 <- newTVarIO 0
   x4 <- newTVarIO 0
   lp x1 x2 x3 x4 i
   f <- atomically (do
                c1 <- readTVar x1
                c2 <- readTVar x2
                c3 <- readTVar x3
                c4 <- readTVar x4
                return (c1,c2,c3,c4))
   print f

   
lp _ _ _ _ 0 = return ()   
lp x1 x2 x3 x4 i = do   
   atomically $ (orElseNestedTest x1 x2 x3 x4)
   lp x1 x2 x3 x4 (i-1)
   
   
   
transaction1 x1 x2 x3 =
  do
    c1 <- readTVar x1
    c2 <- readTVar x2 
    writeTVar x3 (c1 + c2)
    
    
testm =
  do
    x1 <- newTVarIO 1
    x2 <- newTVarIO 10  
    x3 <- newTVarIO 0
    x4 <- newTVarIO 0
    synch <- newMVar 0
    forkIO (someDelay >> atomically (transaction1 x1 x2 x3) >> takeMVar synch >> return ())
    forkIO (someDelay >> atomically (transaction1 x1 x3 x4)  >> takeMVar synch >> return ())   
    putMVar synch 0
    putMVar synch 0
    xs <- atomically ( do 
                   sequence [readTVar x1, readTVar x2, readTVar x3, readTVar x4]
                   )
    print xs
                  
randomRIO :: Integral a => (a, a) -> IO a
randomRIO (l, h) = return $ (l + h) `div` 2
