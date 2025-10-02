module Control.Monad.ST.Unsafe(
  unsafeInterleaveST,
  unsafeDupableInterleaveST,
  unsafeIOToST,
  unsafeSTToIO,
  ) where
import Control.Monad.ST_Type
import System.IO.Unsafe

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST io) = ST (unsafeInterleaveIO io)

unsafeDupableInterleaveST :: ST s a -> ST s a
unsafeDupableInterleaveST = unsafeInterleaveST

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = ST

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO = unST
