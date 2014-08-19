multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Apply a function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zibWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip', take a function, return a function with the parameter inverted
-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--    where g x y = f y x

-- simpler flip
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- quicksort with filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted  = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- largest divisible number under 100.000 divisible by 3829
-- use the lazy property of haskell with head, first result returned by filter,
-- is the one you want
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- sum of all squares that are smaller than 10000
sum_result = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1] -- a Collatz sequences stops at 1
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

-- how many sequences with chain between 1 and 100 which have a length superior
-- at 15
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- lambda are defined with \args -> body with parents all around the lambda fonctions
-- Pattern matching return a runtime error.
lambda_result = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

-- flip, lambda edition
flipLambda :: (a -> b -> c) -> b -> a -> c
flipLambda f = \x y -> f y x

-- sum, foldl edition
sumFoldl :: (Num a) => [a] -> a
sumFoldl = foldl (+) 0

-- elem foldl edition
elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> if x == y then True else acc) False

-- some fold{l,l1,r,r1} exemple
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- How many elements does it take for the sum of the roots of all natural
-- numbers to exceed 1000 ?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- $ is the function application function. Basically like space, but with low
-- precedences
