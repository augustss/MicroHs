module Data.String.Interpolate.ShowS where

interpolateRaw :: String -> ShowS
interpolateRaw = showString

interpolateValue :: Show a => a -> ShowS
interpolateValue = shows

interpolateAppend :: ShowS -> ShowS -> ShowS
interpolateAppend = (.)

interpolateEmpty :: ShowS
interpolateEmpty = id

interpolateFinalize :: ShowS -> ShowS
interpolateFinalize = id
