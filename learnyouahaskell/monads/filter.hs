import Data.Monoid
import Control.Monad.Writer

-- "Automatic" logging and anotating of bool with a Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

-- for the result: fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- for the log: mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

-- Keep and don't keep element of a list at the same time
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
