module Semana01 where

import Prelude hiding ( gcd )

next :: Int -> Int
next x
    | mod x 2 == 0 = div x 2
    | otherwise = 3 * x + 1

steps :: Int -> Int
steps x 
    | x == 1 = 1
    | otherwise = steps (next x)

stepsCounter :: Int -> (Int, Int)
stepsCounter x = stepsCounter' (x, 0)

stepsCounter' :: (Int, Int) -> (Int, Int)
stepsCounter'(x, counter)
    | x == 1 = (x, counter)
    | otherwise = stepsCounter'(next x, counter+1)
    
stepsList :: Int -> ([Int], Int)
stepsList x = stepsList' ([x], 0)

stepsList' :: ([Int], Int) -> ([Int], Int)
stepsList' (x:xs, counter)
    | x == 1 = (x:xs, counter)
    | otherwise = stepsList'((next x) : x : xs, counter+1)

gcd :: Int -> Int -> Int
gcd a b
    | b == 0 = a
    | otherwise = gcd b (mod a b)

gcdCounter :: Int -> Int -> (Int, Int)
gcdCounter a b = gcd' a b 0

gcd' :: Int -> Int -> Int -> (Int, Int)
gcd' a b counter
    | b == 0 = (a, counter)
    | otherwise = gcd' b (mod a b) (counter+1)

average :: Double -> Double -> Double
average a b = (a + b) / 2

goodEnough :: Double -> Double -> Bool
goodEnough guess x = abs(guess ^^ 2 - x) < 0.001

improve :: Double -> Double -> Double
improve guess x = average guess (x / guess)

sqrtIter :: Double -> Double -> Double
sqrtIter guess root
    | goodEnough guess root = guess
    | otherwise = sqrtIter (improve guess root) root

improveC :: Double -> Double -> Double
improveC guess root =  ((root / (guess ^^ 2)) - (2 * guess)) / 3

cubeEnough :: Double -> Double -> Bool
cubeEnough guess x = abs(guess ^^ 3 - x) < 0.0001

cubicIter :: Double -> Double -> Double
cubicIter guess root
    | cubeEnough guess root = guess
    | otherwise = cubicIter (improveC guess root) root

squareSum :: Int -> Int
squareSum n = sum [x^2 | x <- take n [1..n]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- take n [0..n], 
                    y <- take m [0..m]]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y |  x <- xs, y <- ys]
