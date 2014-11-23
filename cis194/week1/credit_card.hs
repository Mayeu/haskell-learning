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
doubleEveryOther ns = reverse $ mapEvery 2 (*2) (reverse ns)

-- Map a function every n element
-- Found here:
-- http://stackoverflow.com/questions/26040420/in-haskell-what-is-the-most-common-way-to-apply-a-function-to-every-nth-element
-- Basically this zip the list of element with a list of id function, where
-- every Nth element are the f function. Thus, applying the f function every
-- Nth element
mapEvery :: Int -> (a -> a) -> [a] -> [a]
mapEvery n f = zipWith ($) (drop 1 . cycle . take n $ f : repeat id)

-- Sum all digit
sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ map (sum . toDigits) ns

-- Validate a credit card number
validate :: Integer -> Bool
validate n = ((sumDigits . doubleEveryOther . toDigits) n `rem` 10) == 0
