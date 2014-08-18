multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Apply a function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zibWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
