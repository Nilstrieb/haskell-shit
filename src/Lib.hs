{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc,
  )
where

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
replaceMultiple (x:xs) c r
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