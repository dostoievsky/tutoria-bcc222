module Semana2 where
import Data.Char
inRange :: Int -> Int -> [Int] -> [Int]
inRange n m xs =  [x | x <- xs, (x > n), (x < m)]

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec n m (x : xs) 
    | (x > n), (x < m) = inRangeRec n m (x : xs)
    | otherwise = inRangeRec n m xs

propInRange :: Int -> Int -> [Int] -> Bool
propInRange n m xs
    | inRange n m xs == inRangeRec n m xs = True
    | otherwise = False

countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, (x > 0)]

countPositivesRec :: [Int] -> Int
countPositivesRec xs = countPositivesRec' xs 0

countPositivesRec' :: [Int] -> Int -> Int
countPositivesRec' [] ac = ac
countPositivesRec' (x : xs) ac 
    | x > 0 = countPositivesRec' xs ac+1
    | otherwise = countPositivesRec' xs ac

propCountPositive :: [Int] -> Bool
propCountPositive xs
    | countPositives xs == countPositivesRec xs = True
    | otherwise = False

toTitleString :: String -> String
toTitleString title = toTitleString' title 0

toTitleString' :: String -> Int -> String
toTitleString' [] _ = []
toTitleString' (t : ts) ac
    | ac == 0 =  toUpper t : toTitleString' ts (ac + 1)
    | otherwise = toLower t : toTitleString' ts (ac + 1)

halfEvens :: [Int] -> [Int]
halfEvens xs = [div x 2 | x <- xs, even x]

halfEvensRec :: [Int] -> [Int]
halfEvensRec [] = []
halfEvensRec (x : xs)
    | even x = div x 2 : halfEvensRec xs 
    | otherwise = halfEvensRec xs

propHalfEvens :: [Int] -> Bool
propHalfEvens xs
    | halfEvens xs == halfEvensRec xs = True
    | otherwise = False

uppers :: String -> String
uppers text = map toUpper text 

centsToReals :: [Int] -> [Float]
centsToReals cs = [fromIntegral c / 100 | c <- cs]

alphas :: String -> String
alphas text = filter (not . isDigit) text

above :: Int -> [Int] -> [Int]
above from list = filter (> from) list
    