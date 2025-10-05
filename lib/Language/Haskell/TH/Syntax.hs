-- This is a dummy module so deriving Lift can be spoofed.
module Language.Haskell.TH.Syntax(Lift(..), lift) where
class Lift a
lift :: a
lift = error "Language.Haskell.TH.Syntax: not implemented"
