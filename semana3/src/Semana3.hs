module Semana3 where

type Matrix = [[Float]]

example :: Matrix
example = [[1,4,9],
           [2,5,7]]

uniform :: Integral a => [a] -> Bool
uniform [] = True
uniform (x : xs) = all (== x) xs 

valid :: Matrix -> Bool
valid [] = False
valid (m : []) = True
valid ms = uniform (map (\ m -> length m) ms) 

dimension :: Matrix -> (Int, Int)
dimension [] = (0, 0)
dimension (m : ms) =  (length ms+1, length m)

square :: Matrix -> Bool
square [] = False
square ms = check (dimension ms)
    where
        check (x, y) = if x == y then True else False 

idMatrix :: Int -> Matrix
idMatrix 0 = []
idMatrix n = [take n [fromIntegral n], take n [fromIntegral n]]