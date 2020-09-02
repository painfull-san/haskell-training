module Cipher where

import Data.Char

toCaesar :: Int -> String -> String
toCaesar n = map (chr . (+96) . flip mod 26 . subtract 96 . (+n) . ord)


unCaesar :: Int -> String -> String
unCaesar n = toCaesar (-n)
  
