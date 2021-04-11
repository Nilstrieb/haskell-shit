{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc,
  )
where

import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

reverseList :: [a] -> [a]
reverseList [x] = [x]
reverseList xs = last xs : reverseList (init xs)

-- | This is just the default map Function
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f xs = [f x | x <- xs]

-- | Double a List
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList xs = mapList (* 2) xs

-- | Replaces all occurrences of a char in a string with another char
replace :: String -> Char -> Char -> String
replace [] _ _ = []
replace [x] c r
  | x == c = [r]
  | otherwise = [x]
replace (x : xs) c r
  | x == c = r : replace xs c r
  | otherwise = x : replace xs c r

replaceMultiple :: String -> [Char] -> Char -> String
replaceMultiple [] _ _ = []
replaceMultiple [x] c r
  | x `elem` c = [r]
  | otherwise = [x]
replaceMultiple (x : xs) c r
  | x `elem` c = r : replaceMultiple xs c r
  | otherwise = x : replaceMultiple xs c r

uwufy :: String -> String
uwufy xs = replaceMultiple xs "rl" 'w'

-- Get all factors of a number
factor :: Int -> [Int]
factor n = getFactors n 1

-- Get all factors of a number x counting up from y
getFactors :: Int -> Int -> [Int]
getFactors x y
  | x `div` 2 < y = [x]
  | x `mod` y == 0 = y : getFactors x (y + 1)
  | otherwise = getFactors x (y + 1)

-- Get all prime factors for a number
primeFactor :: Int -> [Int]
primeFactor n = getPrimeFactors n 2

-- Get all prime factors of a number n counting up from m
getPrimeFactors :: Int -> Int -> [Int]
getPrimeFactors n m
  | n `div` 2 < m = [n]
  | n `mod` m == 0 = m : getPrimeFactors (n `div` m) 2
  | otherwise = getPrimeFactors n (m + 1)

checksum :: Int -> Int
checksum 0 = 0
checksum n = (n `mod` 10) + checksum (n `div` 10)

sumList :: [Int] -> Int
sumList [] = 0
sumList [x] = x
sumList (x : xs) = x + sumList xs

sumListF :: [Int] -> Int
sumListF = foldl (+) 0

{-
'sumListF xs = foldl (\a n -> a + n) 0 xs' can be eta reduced to 'sumListF = foldl (\a n -> a + n) 0'
why? due to currying, 'foldl (\a n -> a + n) 0' returns a function that is then applied to xs
sp instead of applying a function to a value that just applies another function to that value, we can just let that
outer function be the inner function directly

the lambda expression '(\a n -> a + n)' can also be changed to (+)
why? the lambda is a function that takes two inputs and adds them, exactly the same as (+)

this can be further reduced to sum but like no
-}

fizzBuzz :: IO ()
fizzBuzz = putStrLn $ intercalate "\n" [fizzBuzzSingle n | n <- [1 .. 100]]

fizzBuzzSingle :: Int -> String
fizzBuzzSingle n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

-- connect a string list so that it overlaps the strings
-- ["hi", "india", "ares", "resolution"] -> "hindiaresolution"
connectStrings :: [String] -> String
connectStrings [] = []
connectStrings [x] = x
connectStrings (x : xs) = connect2Strings x $ connectStrings xs

connect2Strings :: String -> String -> String
connect2Strings = connect []
  where
    connect _ [] _ = []
    connect w x y
      | x `isPrefixOf` y = w ++ y
      | otherwise = connect (w ++ [head x]) (tail x) y
