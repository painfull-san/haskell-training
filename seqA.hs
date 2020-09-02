seqA :: Integer -> Integer



seqA n 
     | n == 0 = 1
     | n == 1 = 2
     | n == 2 = 3 
     | otherwise = helper 3 2 1 n     
     where helper n3 _ _ 2 = n3
           helper n3 n2 n1 n = helper (n3 + n2 - 2 * n1) n3 n2 (n-1)


{-
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = seqA1 (n - 1) + seqA1 (n - 2) - (2 * seqA1 (n - 3))
-}