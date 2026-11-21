{-# LANGUAGE NoFieldSelectors #-}
module ClassFieldExportDefs (OptionSet(..), Options(..)) where

class Options a where
  options :: OptionSet a

data OptionSet a = OptionSet { options :: [a] }
