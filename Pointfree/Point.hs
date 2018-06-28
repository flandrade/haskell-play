module Point where

getValue1 :: [Integer] -> Integer -> [Integer]
getValue1 list value = tail (take 5 (map (value+) list))

getValue2 :: [Integer] -> Integer -> [Integer]
getValue2 list value = tail $ take 5 $ map (value+) list

getValue3 :: [Integer] -> Integer -> [Integer]
getValue3 list value = tail . take 5 $ map (value+) list

fx ::  Eq a => a -> [a] -> Int
fx x list = length $ filter (== x) list

fx2::  Eq a => a -> [a] -> Int
fx2 x list = length (filter (== x) list)

fx3 ::  Eq a => a -> [a] -> Int
fx3 x = length . filter (== x)
