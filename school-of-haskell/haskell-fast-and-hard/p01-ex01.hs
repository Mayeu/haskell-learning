main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "Where do you live?"
    place <- getLine
    putStrLn $ name ++ " live in " ++ place ++ "."
