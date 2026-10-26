module Error21() where
data T = C Int
x = case undefined of
      C _ _ -> True
