g :: Int -> Int -> Int
g x y = x*x - y*y + x - y

-- should display 6 and -8
main :: IO ()
main = do
    print $ g 3 2.4
    print $ g 3 4
