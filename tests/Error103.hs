module Error103(f) where
f (x :: forall a . a -> a) = x
