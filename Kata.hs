module Kata where

isDivisible :: Int -> Int -> Int -> Bool
isDivisible n x y = if (n `rem` x) == 0 && (n `rem` y) == 0
                    then True
                    else False