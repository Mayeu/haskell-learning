import Data.Monoid

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just (acc + x)

-- foldM binSmalls 0 [2,8,3,1]
-- foldM binSmalls 0 [2,11,3,1]
