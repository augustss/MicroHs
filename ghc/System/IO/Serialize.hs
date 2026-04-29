-- Copyright 2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.Serialize(
  hSerialize, hDeserialize,
  writeSerialized,
  writeSerializedCompressed,
  readSerialized, readSerializedH, readSerializedBS,
  ) where
import Data.ByteString(ByteString)
import System.IO
import GHC.Stack

writeSerializedCompressed :: forall a . HasCallStack => FilePath -> a -> IO ()
writeSerializedCompressed = errghc

writeSerialized :: forall a . HasCallStack => FilePath -> a -> IO ()
writeSerialized = errghc

readSerialized :: forall a . HasCallStack => FilePath -> IO a
readSerialized = errghc

readSerializedH :: forall a . HasCallStack => Handle -> IO a
readSerializedH = errghc

readSerializedBS :: forall a . HasCallStack => ByteString -> IO a
readSerializedBS = errghc

hSerialize   :: forall a . Handle -> a -> IO ()
hSerialize = errghc

hDeserialize :: forall a . Handle -> IO a
hDeserialize = errghc

errghc :: HasCallStack => a
errghc = error "System.IO.System: serialization not available with ghc"
