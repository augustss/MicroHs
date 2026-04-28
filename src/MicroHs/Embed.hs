module MicroHs.Embed where
import Data.ByteString(ByteString)

-- This list is filled by the compiler when compiled with
-- the --embed-packages PKG:PKG:... flag.
-- It is simply the contents of the corresponding .pkg file.
packages :: [ByteString]
packages = []
