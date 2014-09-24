-- Implementing square root calculation with Newton's method

import Data.List (find)

refineSqrtGuess :: Double -> Double -> Maybe Double
refineSqrtGuess x initial = find (acceptGuess x) (guesses x initial)

acceptGuess :: Double -> Double -> Bool
acceptGuess x guess = abs (guess * guess - x) < precision * x
  where precision = 0.001

guesses :: Double -> Double -> [Double]
guesses x initial = iterate (next x) initial

next :: Double -> Double -> Double
next x guess = (guess + x / guess) / 2
