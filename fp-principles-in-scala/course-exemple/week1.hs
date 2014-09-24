-- Implementing square root calculation with Newton's method

sqrt' :: Double -> Double -> Double
sqrt' x guess
  | guessIsGoodEnough = guess
  | otherwise         = sqrt' x improvedGuess
  where
    -- Test the precision of the guess
    precision          = 0.001
    guessIsGoodEnough  = abs (guess * guess - x) < precision * x
    -- Improve our guessed square
    improvedGuess      = (guess + x / guess) / 2
