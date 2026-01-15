module Data.Text.Internal (Text(..)) where

import qualified Prelude ()
import qualified Data.ByteString.Internal as BS
import Data.Typeable

newtype Text = T BS.ByteString
