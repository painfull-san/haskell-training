import Data.Char
import Data.Time
import GHC.Types (Any)
import Data.List

finalComp = listByTwo . addUnter
  where addUnter :: String -> String
        addUnter x = if even (length x) then x else x ++ "_"
        listByTwo :: [a] -> [[a]]
        listByTwo []         = []
        listByTwo (x1:x2:xs) = [x1, x2] : listByTwo xs

eftBool :: Bool -> Bool -> [Bool]
eftBool x y 
         | x == y    = [y]
         | otherwise = x : eftBool (succ x) y

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y 
         | x == y    = [y]
         | otherwise = x : eftOrd (succ x) y

eftInt :: Int -> Int -> [Int]
eftInt x y 
         | x > y    = []
         | otherwise = x : eftInt (x + 1) y

eftChar :: Char -> Char -> [Char]
eftChar x y 
         | x > y     = []
         | otherwise = x : eftChar (succ x) y


myFilterArt :: String -> [String]
myFilterArt x = filter (\x -> x `notElem` ["the", "a", "an"]) $ words x

mip [] _          = []
mip (x:xs) (y:ys) = (x, y) : mip xs ys


mipWith _ [] _          = []
mipWith f (x:xs) (y:ys) = f x y : mipWith f xs ys


-- myOr :: [Bool] -> Bool
-- myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> b || f a) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = (x == y) || myElem x ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse x  = last x : myReverse (init x)

{-
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs
-}

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = m f xs x where
    m :: (a -> a -> Ordering) -> [a] -> a -> a
    m _ [] y = y
    m f (x : xs) y = m f xs (if f x y == GT then x else y) -- здесь получаются два первых члена списка и возвращается больший для дальнейшего рекурсивного вызова

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = m f xs x where
    m :: (a -> a -> Ordering) -> [a] -> a -> a
    m _ [] y = y
    m f (x : xs) y = m f xs (if f x y == LT then x else y)


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                         deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ 
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f [] where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate x) xs = x : xs
    f _ xs          = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f [] where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f (DbNumber x) xs = x : xs
    f _ xs            = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb) xs / (fromIntegral . length . filterDbNumber) xs

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

lessHundrFibs :: [Integer]
lessHundrFibs = takeWhile (<100) fibs 

fibsN :: Int -> Integer
fibsN x = fibs !! x

anotherFact :: [Integer]
anotherFact = scanl (*) 1 [1..]


stops = "pbtdkg"
vowels = "aeiou"

func :: [(Char, Char, Char)]
func = [(x, y, z) | x <- stops, x == 'p', y <- vowels, z <- stops]

myReverse1 :: [a] -> [a]
myReverse1 = foldl (flip (:)) []

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x = myAny (== x)

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = foldr (\a acc -> acc || a == x) False

-- myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

-- squish1 :: [[a]] -> [a]
-- squish1 = foldr (++) []

squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 f = foldr (\a b -> f a ++ b) []

squishAgain1 :: [[a]] -> [a]
squishAgain1 = squishMap1 id

myMaximumBy1 :: Num a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy1 f (x:xs) = foldl (\a b-> if f a b == GT then a else b) x xs

myMinimumBy1 :: Num a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy1 f (x:xs) = foldl (\a b-> if f a b == LT then a else b) x xs

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                    deriving (Eq, Show)


mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
                | name /= "" && age > 0 = Right $ Person name age
                | name == "" = Left NameEmpty
                | age <= 0 = Left AgeTooLow
                | otherwise = Left $ PersonInvalidUnknown $
                                    "Name was: " ++ show name ++
                                    " Age was: " ++ show age

showResult :: Either PersonInvalid Person -> String
showResult (Left error) = "Error occured: " ++ show error
showResult (Right person) = "Yay! Successfully got a person: " ++ show person

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Please enter a person name:"
    name <- getLine
    putStrLn "Please enter a person age:"
    age <- getLine
    putStrLn (showResult (mkPerson name (read age)))

evenFibs :: Int -> [Integer]
evenFibs n = take n (filter even fibs)

isValid :: [Char] -> Bool
isValid = loop []
  where
    match '(' ')' = True
    match '{' '}' = True
    match '[' ']' = True
    match  _   _  = False

    loop st [] = null st
    loop st (x:xs)
      | x `elem` "([{" = loop (x:st) xs
      | x `elem` ")]}" = case st of
        open : st' | match open x -> loop st' xs
        _ -> False -- unmatched close
      | otherwise = loop st xs

data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc $ toNat (x - 1)

add :: Nat -> Nat -> Nat
add a b = toNat (fromNat a + fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat (fromNat a * fromNat b)

fac :: Nat -> Nat
fac Zero = Suc Zero
fac x = mul (fac (toNat(fromNat x - 1))) x

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed


-- Persistent bugger on Codewars
persistence :: Int -> Int
persistence = help 0 where
    multi num = product (map digitToInt (show num))
    help counter num
       | div num 10 == 0 = counter
       | otherwise = help (counter + 1) (multi num)
-- best practices of Persistent bugger
persistence' :: Int -> Int
persistence' n = if n < 10 then 0 else 1 + persistence' (product $ map digitToInt $ show n)


-- Count the digit on Codewars
nbDig :: Int -> Int -> Int
nbDig n d = sum $ map (convBool . (== intToDigit d)) (concatMap (show . (^ 2)) [0 .. n]) 
    where
      convBool a = if a then 1 else 0

-- better solution
nbDig' :: Int -> Int -> Int
nbDig' n d = sum [ length $ filter (==c) $ show (x*x) | x <- [0..n] ]
  where c = intToDigit d

-- Rotate for a Max from Codewars (я не смог сам, 4 ошибочных ответа из 31 и очень громоздкое)
maxRot :: Integer -> Integer
maxRot = read . go . show
  where go n@[_] = n
        go n@(x1:x2:xs) = max n (x2:go (xs ++ [x1]))



digs :: Integral x => x -> [x] -- функция превращения числа в список цифр
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

collatz :: (Num a2, Integral a1) => a1 -> a2 -- функция нарушающая п.4 правил рекурсии выше в случае нечётного числа
collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n*3 + 1)