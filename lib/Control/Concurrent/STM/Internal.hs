-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SHFSTM.Internal
-- Copyright   :  (c) D. Sabel, Goethe-University, Frankfurt a.M., Germany
-- License     :  BSD-style 
-- 
-- Maintainer  :  sabel <at> ki.cs.uni-frankfurt.de
-- Stability   :  experimental
-- Portability :  non-portable (needs GHC and extensions)
--
--  
-- This module implements transaction execution
-----------------------------------------------------------------------------

module Control.Concurrent.STM.Internal (
  TLOG,
  emptyTLOG,
  RetryException(..),
  globalRetry,
  commit,
  newTVarWithLog,
  readTVarWithLog,
  writeTVarWithLog,
  orElseWithLog,
  orRetryWithLog
  ) where

import Prelude hiding(catch)
import Control.Exception 
import Control.Concurrent
import Control.Concurrent.STM.Internal.TVar  
import Control.Concurrent.STM.Internal.TransactionLog
import qualified Control.Concurrent.STM.Internal.Map as Map
import qualified Control.Concurrent.STM.Internal.Set as Set
import Data.List
import Data.IORef
import Data.Maybe
import Data.Typeable     
-- | The 'RetryException' is thrown from the committing transaction
-- to conflicting transactions




  
data RetryException = RetryException 
     deriving (Typeable,Show)

instance Exception RetryException

-- | 'newTVarWithLog' creates a new TVar
newTVarWithLog :: TLOG      -- ^ the transaction log
               -> a         -- ^ the content of the TVar
               -> IO (TVar a)
newTVarWithLog (TLOG tlog) content =
  uninterruptibleMask_ $
   do
    mid <- myThreadId
    tvar_Id <- nextCounter
    lg <- readIORef tlog                   -- access the Transaction-Log
    let ((la,ln,lw):xs) = tripelStack lg   -- access the La,Ln,Lw lists
    -- Create the TVar  ...
    content_global <- newMVar content       -- set global content
    pointer_local_content <- newIORef [content]
    let mp = Map.insert mid pointer_local_content Map.empty
    content_local   <- seq mp (newMVar mp)
    notify_list     <- newMVar Set.empty
    unset_lock      <- newEmptyMVar
    -- loc <- newIORef (Just mid)
    content_waiting_queue <- newMVar []                  -- empty broadcast list
    content_tvarx <-         newMVar (TV {globalContent = content_global,
                                  localContent  = content_local,
                                  notifyList    = notify_list,
                                  lock          = unset_lock,   
                                  waitingQueue  = content_waiting_queue
                                  -- local = loc
                                  })
                                         
    let tvany = TVarAny (tvar_Id,content_tvarx)
    let tva   = TVarA content_tvarx
    let tvar = TVar (tva,tvany)
    -- ------------- 
    writeIORef tlog (lg{tripelStack=(Set.insert tvany la,Set.insert tvany ln,lw):xs}) -- adjust the Transaction Log
    return tvar

    

-- | 'readTVarWithLog' performs the readTVar-operation
readTVarWithLog :: TLOG   -- ^ the transaction log
                -> TVar a -- ^ the  'TVar'
                -> IO a   -- ^ the content of the 'TVar' (local)
readTVarWithLog (TLOG tlog) ptvar@(TVar (_,tvany)) =
 do
  
  res <- tryReadTVarWithLog (TLOG tlog) ptvar
  case res of
    Right r -> return r
    Left blockvar -> do
                     mid <- myThreadId
                     takeMVar blockvar
                     readTVarWithLog (TLOG tlog) ptvar
    
tryReadTVarWithLog (TLOG tlog) ptvar@(TVar (TVarA tva,tvany@(TVarAny tx))) = uninterruptibleMask_ $
  do
    _tva <-  takeMVar tva  -- access the TVar
    lg <- readIORef tlog  -- access the Log-File
    let ((la,ln,lw):xs) = tripelStack lg
    mid <- myThreadId        -- the ThreadId
    if tvany `Set.member` la then do
      -- x in L_a, local copy exists
      uninterruptibleMask_ (
       do       
        localmap <- readMVar  (localContent _tva)
        lk <- readIORef $ fromJust $ Map.lookup mid localmap
        let (x:xs) = lk
        putMVar tva _tva
        return (Right x)        
        )
      else                  
      -- TVar not in read TVars
       do
        uninterruptibleMask_ ( 
         do 
          b <- isEmptyMVar (lock _tva)
          if b -- not locked
           then 
                do
                 nl <- takeMVar (notifyList _tva) 
                 putMVar (notifyList _tva) (Set.insert mid nl) -- add to notifyList
                 globalC <- readMVar  (globalContent _tva) -- read global content
                 content_local <- newIORef [globalC]
                 mp <- takeMVar (localContent _tva)
                 writeIORef tlog (lg{readTVars = Set.insert tvany (readTVars lg),tripelStack = (Set.insert tvany la,ln,lw):xs})
                 putMVar (localContent _tva) (Map.insert mid content_local mp)        -- copy to local tvar stack
                             -- adjust the transaction log
                 putMVar tva _tva
                 return (Right globalC)
      
           else -- locked
                do 
                 blockvar <- newEmptyMVar
                 wq <- takeMVar (waitingQueue _tva)
                 putMVar (waitingQueue _tva) (blockvar:wq)
                 putMVar tva _tva
                 return (Left blockvar)
           )

-- | 'writeTVarWithLog' performs the writeTVar operation and 
--   adjusts the transaction log accordingly
writeTVarWithLog :: TLOG   -- ^ the transaction log
                  -> TVar a -- ^ the  'TVar'
                  -> a      -- ^ the new content
                  -> IO ()  

writeTVarWithLog (TLOG tlog) ptvar@(TVar (_,tvany)) con =
 do
  res <- tryWriteTVarWithLog (TLOG tlog) ptvar con
  case res of
    Right r -> return r
    Left blockvar -> do
                      takeMVar blockvar
                      writeTVarWithLog (TLOG tlog) ptvar con
   
tryWriteTVarWithLog (TLOG tlog) ptvar@(TVar (TVarA tva,tvany@(TVarAny (id,m)))) con =
 uninterruptibleMask_ $ do
    _tva <-  takeMVar tva  -- access the TVar
    lg <- readIORef tlog  -- access the Log-File
    let ((la,ln,lw):xs) = tripelStack lg
    mid <- myThreadId        -- the ThreadId
    if tvany `Set.member` la then do
      -- x in L_a, local copy exists
      uninterruptibleMask_ (
       do       
        localmap <- readMVar  (localContent _tva)
        let ioref_with_old_content = fromJust $ Map.lookup mid localmap
        lk <- readIORef ioref_with_old_content 
        let (x:ys) = lk
        writeIORef  ioref_with_old_content (con:ys)
        writeIORef tlog (lg{tripelStack = (la,ln,Set.insert tvany lw):xs})
        putMVar tva _tva
        return $ Right ()
        )
     else                  
      -- TVar not in read TVars
       do
        uninterruptibleMask_ ( 
         do 
          b <- isEmptyMVar (lock _tva)
          if b -- not locked
           then 
                do
                 globalC <- readMVar  (globalContent _tva) -- read global content
                 content_local <- newIORef [con]
                 mp <- takeMVar (localContent _tva)
                 putMVar (localContent _tva) (Map.insert mid content_local mp)        -- copy to local tvar stack
                             -- adjust the transaction log
                 writeIORef tlog (lg{readTVars = Set.insert tvany (readTVars lg),tripelStack = (Set.insert tvany la,ln,Set.insert tvany lw):xs})
                 putMVar tva _tva
                 return (Right ())
      
           else -- locked
                do 
                 blockvar <- newEmptyMVar
                 wq <- takeMVar (waitingQueue _tva)
                 putMVar (waitingQueue _tva) (wq ++ [blockvar])
                 putMVar tva _tva
                 return (Left blockvar)

           )


           
writeStartWithLog' (TLOG tlog) =
 uninterruptibleMask_ $
   do
    mid <- myThreadId    
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):_)  = tripelStack lg
    let t               = readTVars lg
    let xs = t `Set.union` (la Set.\\ ln)  -- x1,...,xn
    res <- uninterruptibleMask_ (grabLocks mid (Data.List.sort $ Set.elems xs)  [])
    case res of
       Right _ -> do 
                   writeIORef tlog (lg{lockingSet = xs}) -- K := xs
                   return (Right ())
       Left lock -> return (Left lock)
    
-- | 'writeStartWithLog' starts the commit phase, by 
-- locking the read and written TVars

writeStartWithLog :: TLOG -- ^ the transaction log
                     -> IO () 
writeStartWithLog (TLOG tlog) =
  do
    res <- writeStartWithLog' (TLOG tlog)
    case res of 
     Left lock -> 
      do
       mid <- myThreadId
       takeMVar lock
       yield
       threadDelay 1000
       writeStartWithLog (TLOG tlog) 
     Right () -> return ()
     
grabLocks mid [] _  = return (Right ())     
grabLocks mid (ptvar@(TVarAny (i,tvany)):xs) held = 
  uninterruptibleMask_ $ do
          _tvany <- takeMVar tvany
          b <- tryPutMVar (lock _tvany) mid
          if b -- not locked
           then  do
             putMVar tvany _tvany
             grabLocks mid xs (ptvar:held)
            
            else  do -- already locked
             waiton <- newEmptyMVar
             l <- takeMVar (waitingQueue _tvany)
             putMVar (waitingQueue _tvany) (l ++ [waiton])
             putMVar tvany _tvany
             mapM_ (\(TVarAny (i,tvany)) -> do
                                                          _tv <- takeMVar tvany
                                                          takeMVar (lock _tv)
                                                          putMVar tvany _tv) (reverse held)
                                                          
             return (Left waiton)
                          
     
    
           
           
           
           
iterateClearWithLog (TLOG tlog) [] = return ()
iterateClearWithLog (TLOG tlog) ((TVarAny (id,tvany)):xs) =
  do
    mid <- myThreadId
    uninterruptibleMask_ $
      do 
       lg <- readIORef tlog
       _tvany <- takeMVar tvany
       ns <- takeMVar (notifyList _tvany)
       -- remove thread id from notify list:
       putMVar  (notifyList _tvany) (Set.delete mid ns)
       putMVar tvany _tvany
    iterateClearWithLog (TLOG tlog) xs    
-- iterate clearWrite as long as possible (i.e. until the T-List is empty)
-- Note: the iteration is not atomic (as in the paper)

-- | 'writeClearWithLog' removes the notify entries of the committing transaction    
writeClearWithLog (TLOG tlog) =
  do 
    lg <- readIORef tlog  -- access the Log-File
    let xs =  Set.elems (readTVars lg)
    iterateClearWithLog (TLOG tlog) xs
    writeIORef tlog (lg{readTVars=Set.empty})

     
           
getIds []                       ls = return (Set.elems ls)          
getIds ((TVarAny (_,tvany)):xs) ls =
 do
  l <- uninterruptibleMask_
   (do 
     _tvany <- takeMVar tvany 
     l <- takeMVar (notifyList _tvany)
     putMVar (notifyList _tvany) Set.empty
     putMVar tvany _tvany
     return l
    )
  getIds xs (Set.union l ls)
-- | 'sendRetryWithLog' sends exceptions to the conflicting threads
  
sendRetryWithLog :: TLOG -- ^ the transaction log
                 -> IO ()
                 
sendRetryWithLog (TLOG tlog) =
  uninterruptibleMask_ $ 
   do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg
    openLW <- getIds (Set.elems lw) Set.empty
    notify openLW

notify [] = return ()
notify (tid:xs) =
 do        
   mid <- myThreadId
   -- ack <- newEmptyMVar 
   throwTo tid RetryException -- send retry, 
   notify xs

           
-- | 'writeTVWithLog' performs the write-Operations of the committing thread.
writeTVWithLog :: TLOG -- ^ the transaction log
               -> IO ()
               
writeTVWithLog (TLOG tlog) =
  do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg
    let tobewritten = lw Set.\\ ln
    writeTVars (Set.elems tobewritten)
    writeIORef tlog lg{tripelStack=(la,ln,Set.empty):xs}

writeTVars [] = return ()
writeTVars ((TVarAny (tid,tvany)):xs) =
  do
   mid <- myThreadId
   _tvany <- takeMVar tvany
   localMap <- takeMVar (localContent _tvany)
   case Map.lookup mid localMap of
    Just conp -> 
      do
        con <- readIORef conp
        let (ltv:stackedcontent) = con 
        putMVar (localContent _tvany) (Map.delete mid localMap)
        -- copy to global storage
        takeMVar (globalContent _tvany)
        putMVar  (globalContent _tvany) ltv
        putMVar tvany _tvany
        writeTVars xs

   

-- ------------------------------------------------
-- unlockTVWithLog  implements (unlockTV)
-- ------------------------------------------------

-- | 'unlockTVWithLog' remove the locks of the TVars during commit
unlockTVWithLog :: TLOG -- ^ the transaction log
               -> IO ()

unlockTVWithLog (TLOG tlog) =
  uninterruptibleMask_ $ do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg
    let k = lockingSet lg
    unlockTVars (Set.elems k)
    writeIORef tlog lg{lockingSet = Set.empty}
    
unlockTVars [] = return ()
unlockTVars ((TVarAny (i,tvany)):xs) =    
   uninterruptibleMask_ $
    do
       mid <- myThreadId
       _tvany <- takeMVar tvany
       wq <- takeMVar (waitingQueue _tvany)    
       takeMVar (lock _tvany)
       putMVar (waitingQueue _tvany) []
       mapM_ (`putMVar` ()) wq
       putMVar tvany _tvany
       unlockTVars xs

-- | 'writeTVnWithLog' writes the newly created TVars during commit    
writeTVnWithLog :: TLOG -- ^ the transaction log
               -> IO ()

writeTVnWithLog (TLOG tlog) =
  do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg
    let t = readTVars lg
    let k = lockingSet lg
    let toBeWritten = Set.elems ln
    writeNew toBeWritten
    writeIORef tlog lg{tripelStack=(la,Set.empty,lw):xs}
    
writeNew [] = return ()
writeNew ((TVarAny (i,tvany)):xs) =
 do
  mid <- myThreadId
  _tvany <- takeMVar tvany
  lmap <- takeMVar (localContent _tvany)
  if Map.null (Map.delete mid lmap) then
   case Map.lookup mid lmap of
    Just conp ->
      do 
       (con:_) <- readIORef conp
       takeMVar (globalContent _tvany)
       putMVar (globalContent _tvany) con
       putMVar (localContent _tvany) Map.empty
       putMVar tvany _tvany
       writeNew xs
    else 
       error "panic"
       
      
-- ------------------------------------------------
-- writeEndWithLog  implements (writeEnd)
-- ------------------------------------------------

-- | 'writeEndWithLog' clears the local TVars, during commit

writeEndWithLog :: TLOG -- ^ the transaction log
               -> IO ()

writeEndWithLog (TLOG tlog) =
  do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg
    let t = readTVars lg
    let k = lockingSet lg
    clearEntries (Set.elems la)

clearEntries [] = return ()
clearEntries ((TVarAny (_,tvany)):xs) =
 do
  mid <- myThreadId
  _tvany <- takeMVar tvany
  localmap <- takeMVar (localContent _tvany)
  putMVar (localContent _tvany) (Map.delete mid localmap)
  putMVar tvany _tvany
  
-- ------------------------------------------------
-- commit performs all the operations for committing
-- ------------------------------------------------


-- | 'commit' performs the operations for committing
--
--   - 'writeStartWithLog' to lock the read and to-be-written TVars
--
--   - 'writeClearWithLog' (iteratively) to remove the notify entries of the committing transaction
--
--   - 'sendRetryWithLog' (iteratively) to abort conflicting transactions
--
--   - 'writeTVWithLog' (iteratively) to write the local contents in to the global memory
--
--   - 'writeTVnWithLog' (iteratively) to create the newly created TVars in the global memory
--
--   - 'writeEndWithLog'  to clear the local TVar stack
--
--   - 'unlockTVWithLog' (iteratively) to unlock the global TVars
 
commit :: TLOG -- ^ the transaction logs
       -> IO () 
commit (TLOG tlog) =
 do
   mid <- myThreadId
--    yield 
   writeStartWithLog (TLOG tlog)       -- writeStart 
--    yield 
   writeClearWithLog (TLOG tlog)     -- clearWrite phase
--    yield 
   sendRetryWithLog  (TLOG tlog) -- sendRetry phase
--    yield 
   writeTVWithLog (TLOG tlog)   -- writeTV phase
--    yield   
   writeTVnWithLog (TLOG tlog) -- writeTVn phase
--    yield    
   writeEndWithLog (TLOG tlog) -- writeTVn phase
   unlockTVWithLog (TLOG tlog) -- unlockTV phase

           
   




    
-- ------------------------------------------------
-- retryCGlobWithLog  implements (retryCGlob)
-- ------------------------------------------------
-- | 'retryCGlobWithLog' performs the removal of notify entries during retry
retryCGlobWithLog :: TLOG -- ^ the transaction log
                  -> IO ()
retryCGlobWithLog (TLOG tlog) =
  do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let t = Set.elems (readTVars lg)
    uninterruptibleMask_ $ 
      do
       removeNotifyEntries mid t
       writeIORef tlog lg{readTVars = Set.empty}
       
removeNotifyEntries mid [] = return ()
removeNotifyEntries mid ((TVarAny (i,tvany)):xs) =
  uninterruptibleMask_ $
   do
    _tvany <- takeMVar tvany
    nlist <- takeMVar (notifyList _tvany)
    putMVar (notifyList _tvany)  (Set.delete mid nlist)
    putMVar tvany _tvany
    removeNotifyEntries mid xs

  

    
-- ------------------------------------------------
-- retryEndWithLog  implements (retryEnd) (slightly modified)
-- ------------------------------------------------
-- | 'retryEndWithLog' resets the transaction log and the local tvar content during retry

retryEndWithLog :: TLOG -- ^ the transaction log
                -> IO ()
retryEndWithLog (TLOG tlog) =
  do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg
    let tvset = la `Set.union` ln `Set.union` lw
    let toBeResetted = Set.elems tvset
    uninterruptibleMask_ $
      do 
       resetEntries mid toBeResetted    
       writeIORef tlog (lg{tripelStack = (Set.empty,Set.empty, Set.empty):xs})
    
resetEntries mid [] = return ()
resetEntries mid ((TVarAny (_,tvany)):xs) =    
  uninterruptibleMask_ $
       do
         _tvany <- takeMVar tvany
         localmap <- takeMVar (localContent _tvany)
         putMVar (localContent _tvany) (Map.delete mid localmap)
         putMVar tvany _tvany
         resetEntries mid xs

         
-- ----------------------------
-- globalRetry should be called when the transaction retries
-------------------------------
-- | 'globalRetry' should be called to retry a transaction, it iteratively 
-- perform 'retryCGlobWithLog' and then 'retryEndWithLog'

globalRetry :: TLOG -- ^ the transaction log
            -> IO () 
globalRetry (TLOG tlog) =
 catch (
  do
   mid <- myThreadId
   uninterruptibleMask_ (retryCGlobWithLog (TLOG tlog))
   uninterruptibleMask_ (retryEndWithLog  (TLOG tlog))
    ) (\ RetryException -> globalRetry (TLOG tlog))
   
-- ------------------------------------------------
-- orElseWithLog (performs (orElse), i.e duplication of stacks etc.
-- ------------------------------------------------

-- | 'orElseWithLog' should be called for the evaluation of 'orElse' to duplicate the local TVar stacks and
--    the entries in the transaction log
orElseWithLog :: TLOG -- ^ the transaction log 
              ->  IO ()
orElseWithLog (TLOG tlog) =
  do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg

    -- double all local TVars
    uninterruptibleMask_ (
      do 
       doubleLocalTVars mid (Set.elems la)
       writeIORef tlog (lg{tripelStack=(la,ln,lw):((la,ln,lw):xs)})
     )
     
doubleLocalTVars mid [] = return ()
doubleLocalTVars mid ((TVarAny (_,tvany)):xs) =
 uninterruptibleMask_ $
   do
    _tvany <- takeMVar tvany
    localmap <- takeMVar (localContent _tvany)
    case Map.lookup mid localmap of
      Just conp -> 
        do 
         (x:ys) <- readIORef conp
         writeIORef conp (x:x:ys)
         putMVar (localContent _tvany) localmap
         putMVar tvany _tvany
         doubleLocalTVars mid xs

-- ------------------------------------------------
-- orRetryWithLog (performs (orRetry), i.e removal
-- ------------------------------------------------

-- | 'orRetryWithLog' should be called when the left expression of an 'orElse' evaluates to 'retry'
--   it pops all stacks (local TVars and transaction log)
orRetryWithLog :: TLOG -- ^ the transaction log
               -> IO ()
orRetryWithLog (TLOG tlog) =
  do
    lg <- readIORef tlog  -- access the Log-File
    mid <- myThreadId
    let ((la,ln,lw):xs) = tripelStack lg
    uninterruptibleMask_ $ 
      do
        undoubleLocalTVars mid (Set.elems la)    
        writeIORef tlog (lg{tripelStack=xs})

    
undoubleLocalTVars mid [] = return []
undoubleLocalTVars mid ((TVarAny (_,tvany)):xs) =
 uninterruptibleMask_ $
    do
      _tvany <- takeMVar tvany
      localmap <- takeMVar (localContent _tvany)
      case Map.lookup mid localmap of
        Just conp -> do
         (l:ltv) <- readIORef conp
         writeIORef conp ltv
         putMVar tvany _tvany
         putMVar (localContent _tvany) localmap
         undoubleLocalTVars mid xs
         
-- *************************************************************************************           
-- only for debugging: generate a global TVar quickly           
newGlobalTVar content =
 do
    tvar_Id <- nextCounter
    -- Create the TVar  ...
    content_global <- newMVar content       -- set global content
    content_local   <- newMVar Map.empty     
    notify_list     <- newMVar Set.empty
    unset_lock      <- newEmptyMVar
    -- loc <- newIORef Nothing
    content_waiting_queue <- newMVar []                  -- empty broadcast list
    content_tvarx <-         newMVar (TV {globalContent = content_global,
                                  localContent  = content_local,
                                  notifyList    = notify_list,
                                  lock          = unset_lock,   
                                  waitingQueue  = content_waiting_queue
                                  -- local = loc
                                  })
    let tvany = TVarAny (tvar_Id,content_tvarx)
    let tva   = TVarA content_tvarx
    let tvar = TVar (tva,tvany)
    return tvar

    


            
