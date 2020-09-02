highAndLow :: String -> String

highAndLow input = show (helperMax input) ++ " " ++ show (helperMin input)

  where helperMax b = maximum (map read $ words b :: [Int])
        helperMin b = minimum (map read $ words b :: [Int])