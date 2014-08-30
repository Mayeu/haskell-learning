import Control.Monad.Instances

-- function are monad too
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)
