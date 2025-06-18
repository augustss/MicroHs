-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SHFSTM.Internal.TVar
-- Copyright   :  (c) D. Sabel, Goethe-University, Frankfurt a.M., Germany
-- License     :  BSD-style 
-- 
-- Maintainer  :  sabel <at> ki.cs.uni-frankfurt.de
-- Stability   :  experimental
-- Portability :  non-portable (needs GHC and extensions)
--
--  
-- This module implements the interface to the STM implementation.
-----------------------------------------------------------------------------


module Control.Concurrent.STM.Internal.TVar (
  -- * Types
  TVar(..),
  TVarA(..),
  TVarAny(..),
  ITVar(..),
  TVarId,
  -- * Operations
  nextCounter
  ) where

import Prelude hiding(catch)
import System.IO.Unsafe
import Data.IORef
import qualified Data.List
import Control.Concurrent.STM.Internal.Map as Map
import Control.Concurrent.STM.Internal.Set as Set
import Control.Concurrent
import Control.Exception
import Data.Maybe
import Data.Typeable
    
-- =============================================================
-- Implementation of the TVar with local and global components
-- Everything is packed by pointers to allow easy (side-effecting) 
-- updates

-- | The 'TVar'-type represents transactional variables
-- Internally it is represented by a pair of transactional variables:
-- a 'TVarA' and a 'TVarAny', both represent the same transactional
-- variable, but 'TVarA' is  explictely polymorphically typed, while
-- the type is hidden in TVarAny
newtype TVar a = TVar (TVarA a,TVarAny)

-- | 'TVarA' is an 'MVar' containing an 'ITVar'
newtype TVarA a = TVarA (MVar (ITVar a))

-- | 'TVarAny' contains an identifier (of type 'TVarId') and
-- and an 'MVar' which contains an 'ITVar'.
-- The content type is hidden using existential types
data  TVarAny = forall a. TVarAny (TVarId, MVar (ITVar a))

-- | 'tvarAToTVarAny' converts a 'TVarA' a into a 'TVar' 
tvarAToTVar :: TVarA a -> TVarId -> IO (TVar a)
tvarAToTVar (TVarA p) tid = 
 do return (TVar (TVarA p, TVarAny (tid,p)))

instance Eq TVarAny where
 (TVarAny (i,_)) == (TVarAny (j,_)) = i == j

instance Ord TVarAny where
 compare (TVarAny (i,_)) (TVarAny (j,_)) = compare i j
 
instance Show TVarAny where
 show (TVarAny (i,_)) = show i

instance Eq (TVar a) where
 (TVar (TVarA l,_)) == (TVar (TVarA r,_)) = l == r

-- | The real content of a TVar is stored in the 'ITVar' type which has five components,
--   all of them are packed into 'MVar's to allow safe access and efficient updates
data ITVar a = 
  TV { 
       -- | The global content of the TVar
      globalContent :: MVar a                                
       -- | The local copies: for every Thread a TVar-Stack (protected by a pointer)
     ,localContent  :: MVar (Map.Map ThreadId (IORef [a])) 
       -- | The notify-List: a set of thread identifiers, for those threads which are inside
       -- transactions and want to be notfied when their state becomes conflicting.
       -- If this case occurs all threads in notifyList will receive an exception                                                     
     ,notifyList    :: MVar (Set.Set ThreadId) -- a channel of thread ids
       -- | The exclusive lock is used to lock the TVar by the committing transaction
     ,lock          :: MVar ThreadId 
       -- | The waitingQueue is used to block, if the TVar is locked, the blocked thread will 
       -- be woken up by the committing thread
     ,waitingQueue  :: MVar [MVar ()]
     }
-- | Identifiers 'TVarId' are 'Integer's. They are only necessary for the 'Ord'-instance of 'TVarAny'
     
type TVarId = Integer


{-# NOINLINE globalCounter #-}
-- | The 'globalCounter' is used to generate new 'TVarId's
globalCounter :: MVar TVarId
globalCounter = unsafePerformIO $ newMVar 0

-- | 'nextCounter' generates a new 'TVarId'
nextCounter :: IO TVarId
nextCounter = 
 mask_ $ do
   i <- takeMVar globalCounter
   putMVar globalCounter (i+1)
   return i

   
