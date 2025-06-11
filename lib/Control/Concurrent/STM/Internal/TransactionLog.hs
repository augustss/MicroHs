-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SHFSTM.Internal.TransactionLog
-- Copyright   :  (c) D. Sabel, Goethe-University, Frankfurt a.M., Germany
-- License     :  BSD-style 
-- 
-- Maintainer  :  sabel <at> ki.cs.uni-frankfurt.de
-- Stability   :  experimental
-- Portability :  non-portable (needs GHC and extensions)
--
--  
-- This module implements the transaction log
-----------------------------------------------------------------------------

module Control.Concurrent.STM.Internal.TransactionLog (
 TLOG(..),
 Log(..),
 emptyTLOG 
 ) where

import Control.Concurrent.STM.Internal.TVar 
import Control.Concurrent.STM.Internal.Set as Set
import Data.IORef
 
-- | The transaction log 'TLOG' of a thread
--   for easy updates it is packed into a pointer
newtype TLOG = TLOG (IORef Log)

-- | The 'Log' stores several lists 
--   it only stores 'TVarAny's

data Log  = 
   Log { -- | List T, the read TVars 
         readTVars :: Set TVarAny
         -- | A stack of tripels (La,Ln,Lw) where 
         --
         --   La = all local tvars,
         --
         --   Ln = new local tvars,
         --
         --   Lw = locally written TVars
       , tripelStack :: [(Set TVarAny, Set TVarAny,Set TVarAny)] 
         -- | List K
       , lockingSet :: Set TVarAny 
   }
 
-- | 'emptyTLOG' constructs an empty transaction log of type 'TLOG'

emptyTLOG :: IO TLOG
emptyTLOG =
  do p <-  newIORef (Log Set.empty [(Set.empty,Set.empty,Set.empty)] Set.empty)
     return (TLOG p)
