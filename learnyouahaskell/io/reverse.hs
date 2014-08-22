main = do
    line <- getLine
    if null line
       then return ()
       else do
           putStrLn' $ reverseWords line
           main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- Implementing putStr' with putChar
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

-- Implementing putStrLn' with putStr'
putStrLn' :: String -> IO ()
putStrLn' s = do
    putStr' s
    putChar '\n'
