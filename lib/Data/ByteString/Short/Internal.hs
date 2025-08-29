-- For GHC compatibility
module Data.ByteString.Short.Internal(
  ShortByteString,
  fromShort,
  toShort,
  ) where
import Data.ByteString

newtype ShortByteString = S ByteString

fromShort :: ShortByteString -> ByteString
fromShort (S b) = b

toShort :: ByteString -> ShortByteString
toShort b = S b
