-- This module is updated by updateversion.sh
module MicroHs.Version(
  version,
  ) where
import qualified Prelude(); import MHSPrelude

version :: Version
version = makeVersion [0,17,3,0]
