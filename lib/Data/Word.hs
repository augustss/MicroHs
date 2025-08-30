-- Copyright 2023,2024,2025 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word(
  Word,
  Word8, bitReverse8,
  Word16, bitReverse16, byteSwap16,
  Word32, bitReverse32, byteSwap32,
  Word64, bitReverse64, byteSwap64,
  ) where
import qualified Prelude()              -- do not import Prelude
import Data.Word.Word
import Data.Word.Word8
import Data.Word.Word16
import Data.Word.Word32
import Data.Word.Word64
