fibonacci :: Integer -> Integer

fibonacci n = fibHelper 0 1 n
   where fibHelper cur prev n
                  | n == 0 = cur
                  | n > 0  = fibHelper (prev + cur) cur (n - 1)
                  | n < 0  = fibHelper prev (cur - prev) (n + 1)

{- 
fibHelper 0 1 -3 = fibHelper 1 1 -2
fibHelper 1 1 -2 = fibHelper 1 (1 + 1) -1
fibHelper 1 2 -1 = fibHelper 2 (2 + 1) 0
-}



fibonacci1 :: Integer -> Integer
fibonacci1 n
    | n == 0 = 0
    | n == 1 = 1
    | n == (-1) = 1
    | n > 0 = fibonacci1 (n - 1) + fibonacci1 (n - 2)
    | n < 0 = fibonacci1 (n + 2) - fibonacci1 (n + 1)
