module MicroHs.Flags(module MicroHs.Flags) where
import Prelude

data Flags = Flags
  Int        -- verbosity level
  Bool       -- run instead of compile
  [String]   -- module search path
  String     -- output file
  Bool       -- show loading message
  Bool       -- use caching
  Bool       -- emit ticks
  deriving (Show)

verbose :: Flags -> Int
verbose (Flags x _ _ _ _ _ _) = x

runIt :: Flags -> Bool
runIt (Flags _ x _ _ _ _ _) = x

paths :: Flags -> [String]
paths (Flags _ _ x _ _ _ _) = x

output :: Flags -> String
output (Flags _ _ _ x _ _ _) = x

loading :: Flags -> Bool
loading (Flags _ _ _ _ x _ _) = x

useCache :: Flags -> Bool
useCache (Flags _ _ _ _ _ x _) = x

useTicks :: Flags -> Bool
useTicks (Flags _ _ _ _ _ _ x) = x
