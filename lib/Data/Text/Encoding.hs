module Data.Text.Encoding (
    -- XXX: add more
    decodeUtf8,
    encodeUtf8,
) where

import qualified Prelude ()
import Control.Error (error)
import Data.Bool (otherwise)
import Data.ByteString (ByteString, isValidUtf8)
import Data.Text
import Unsafe.Coerce (unsafeCoerce)

decodeUtf8 :: ByteString -> Text
decodeUtf8 bs
    | isValidUtf8 bs = unsafeCoerce bs -- Safety: the ByteString is valid UTF-8 and Text uses UTF-8 encoding
    | otherwise = error "Data.Text.Encoding.decodeUtf8: invalid"

encodeUtf8 :: Text -> ByteString
encodeUtf8 = unsafeCoerce -- Safety: Text uses UTF-8 encoding
