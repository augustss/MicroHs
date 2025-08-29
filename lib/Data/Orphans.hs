-- Instance declarations that can't be put where
-- due to import cycles.
module Data.Orphans where
import qualified Prelude(); import MiniPrelude
import Text.Read

instance (Read a) => Read (Down a) where
  readsPrec d = readParen (d > 10) $ \ r ->
    [(Down x,t) | ("Down",s) <- lex r, (x,t) <- readsPrec 11 s]

instance (Show a) => Show (Down a) where
  showsPrec d (Down x) = showParen (d > 10) $
    showString "Down " . showsPrec 11 x
