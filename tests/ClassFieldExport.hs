{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
module ClassFieldExport(main) where

import ClassFieldExportDefs

instance Options Int where
  options = OptionSet [1, 2, 3]

main = print (options @Int).options
