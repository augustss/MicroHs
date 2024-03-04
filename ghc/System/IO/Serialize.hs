-- Copyright 2024 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.Serialize(
{-
  hSerialize, hDeserialize,
  writeSerialized,
-}
  writeSerializedCompressed,
  readSerialized,
  ) where
--import System.IO

{-
hSerialize   :: forall a . Handle -> a -> IO ()
hSerialize = errghc
hDeserialize :: forall a . Handle -> IO a
hDeserialize = errghc
writeSerialized :: forall a . FilePath -> a -> IO ()
writeSerialized = errghc
-}

writeSerializedCompressed :: forall a . FilePath -> a -> IO ()
writeSerializedCompressed = errghc
readSerialized :: forall a . FilePath -> IO a
readSerialized = errghc

errghc :: a
errghc = error "System.IO.System: serialization not available with ghc"
