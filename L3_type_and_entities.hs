h :: Double -> Double
h x = if x <= 0 then 0 else 1

h' :: Double -> Double
h' x = 
    case x <= 0 of
        True -> 0
    --    False -> 1