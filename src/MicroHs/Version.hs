-- This module is updated by updateversion.sh
module MicroHs.Version(
  version,
  ) where
import qualified Prelude()
import Data.Version

version :: Version
version = makeVersion [0,16,5,0]
