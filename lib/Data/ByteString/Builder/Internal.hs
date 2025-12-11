module Data.ByteString.Builder.Internal where

import qualified Prelude ()
import MiniPrelude

import Data.ByteString.Internal (StrictByteString)
import Data.ByteString.Lazy (LazyByteString, append, empty, fromStrict)

newtype Builder = Builder (LazyByteString -> LazyByteString)

instance Semigroup Builder where
    Builder f <> Builder g = Builder (f . g)

instance Monoid Builder where
    mempty = Builder id

instance IsString Builder where
    fromString = stringUtf8

instance Show Builder where
    show = show . toLazyByteString

toLazyByteString :: Builder -> LazyByteString
toLazyByteString (Builder f) = f empty

byteString :: StrictByteString -> Builder
byteString bs = Builder (fromStrict bs `append`)

lazyByteString :: LazyByteString -> Builder
lazyByteString bs = Builder (bs `append`)

stringUtf8 :: String -> Builder
stringUtf8 = byteString . _primitive "toUTF8"
