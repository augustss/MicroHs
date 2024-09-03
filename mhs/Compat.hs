-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Compat(rnfNoErr, rnfErr, NFData, appendDot) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Text
-- So we can import Compat, which is full of stuff for GHC.

-- Define these here to avoid dragging in Control.DeepSeq
rnfNoErr :: forall a . a -> ()
rnfNoErr = primRnfNoErr

rnfErr :: forall a . a -> ()
rnfErr = primRnfErr

class NFData a

appendDot :: Text -> Text -> Text
appendDot x y =
  primitive "bs++." x y
  --x `append` pack "." `append` y

