-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-unused-imports #-}
module MicroHs.Translate(
  TranslateMap, translateMapEmpty,
  translate,
  translateMap,
  translateWithMap,
  translateAndRun
  ) where
import qualified Prelude(); import MHSPrelude
import Data.ByteString(ByteString)
import Data.ByteString.Char8(pack)
import Data.Maybe
import Data.String
import Unsafe.Coerce

import PrimTable
import MicroHs.Desugar(LDef, encodeInteger)
import MicroHs.Expr
import MicroHs.Exp
import MicroHs.ExpPrint(toStringCMdl, renumberCMdlM)
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import System.IO.Serialize(readSerializedBS)
import System.IO.Unsafe(unsafePerformIO)
import Text.PrettyPrint.HughesPJLiteClass(prettyShow)
--import Debug.Trace

newtype TranslateMap = TM (M.Map Exp)

translateMapEmpty :: TranslateMap
translateMapEmpty = TM M.empty

-- Convert the expression e in the environment ds into its value
translate :: ([LDef], Exp) -> AnyType
translate = translateWithMap translateMapEmpty

-- Convert a bunch of definitions to a TranslateMap
translateMap :: [LDef] -> TranslateMap
translateMap = TM . M.fromList

-- Convert and assume the converted value is of type IO() and run it
translateAndRun :: ([LDef], Exp) -> IO ()
translateAndRun defs = do
  let prog = unsafeCoerce (translate defs)
  prog

---------------------------------------------------------------------

-- Translate the Exp to its value by printing it in serailized form
-- and then use the regular runtime deserialization.
translateWithMap :: TranslateMap -> ([LDef], Exp) -> AnyType
translateWithMap _ _ | not compiledWithMhs =
  mhsError "Not compiled with mhs, so cannot run code"
translateWithMap (TM mp) (ds, e) =
  let dm = foldr (uncurry M.insert) mp ds
      outCMdl = snd $ renumberCMdlM [] dm e
      str = toStringCMdl outCMdl    -- serialized value
      val = unsafePerformIO (readSerializedBS (pack str))
  in  val
