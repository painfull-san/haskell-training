module BitOperation where

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

add :: Z -> Z -> Z
add = undefined

mul :: Z -> Z -> Z
mul = undefined

-- Z Plus [Zero, One] = 2

{-

1 + 0 = 1
0 + 1 = 1
0 + 0 = 0
1 + 1 = (2)10

[Zero, One] [Zero, One] = [Zero, Zero, One]

add One One = Zero : One

-}