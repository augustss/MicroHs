module Data.String.Interpolate.Basic where

interpolateRaw :: IsString s => String -> s
interpolateRaw = fromString

interpolateValue :: s -> s
interpolateValue = id

interpolateAppend :: Monoid s => s -> s -> s
interpolateAppend = mappend

interpolateEmpty :: Monoid s => s
interpolateEmpty = mempty

interpolateFinalize :: s -> s
interpolateFinalize = id
