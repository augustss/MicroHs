module Data.ByteString.Builder.Extra where

import qualified Prelude ()
import MiniPrelude

import Control.Monad.ST
import Data.ByteString.Builder
import Data.ByteString.Builder.Internal
import Data.Double
import Data.Float
import Data.Int
import Data.Word
import Foreign.Storable
import Mhs.MutUArr
import Mhs.UArr

flush :: Builder
flush = Builder id

host :: (Storable a) => a -> Builder
host x = runST $ do
    arr <- newMutSTUArr 1
    unsafeWriteMutSTUArr arr 0 x
    UArr bs <- unsafeFreezeMutSTUArr arr
    pure (byteString bs)

intHost :: Int -> Builder
intHost = host

int16Host :: Int16 -> Builder
int16Host = host

int32Host :: Int32 -> Builder
int32Host = host

int64Host :: Int64 -> Builder
int64Host = host

wordHost :: Word -> Builder
wordHost = host

word16Host :: Word16 -> Builder
word16Host = host

word32Host :: Word32 -> Builder
word32Host = host

word64Host :: Word64 -> Builder
word64Host = host

floatHost :: Float -> Builder
floatHost = host

doubleHost :: Double -> Builder
doubleHost = host
