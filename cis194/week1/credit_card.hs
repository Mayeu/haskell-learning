{-# OPTIONS_GHC -Wall #-}

module CreditCard where

-- Split a number into digits, and reverse the list
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Split a positive number into a list of digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | n < 10    = [n]
    | otherwise = toDigits division ++ [reminder]
   where
       reminder = n `rem` 10
       division = n `div` 10

-- Double every other number from the right of the list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [n]      = [n]
doubleEveryOther (n:m:ns) = n : m*2 : doubleEveryOther ns
