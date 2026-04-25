-- This module is updated by updateversion.sh
module MicroHs.Version(
  version,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.Version

version :: Version
version = makeVersion [0,15,6,0]
