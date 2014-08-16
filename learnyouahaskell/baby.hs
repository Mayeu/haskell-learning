-- How to function
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                         then x
                         else x*2

-- List comprehension
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- Idiotic length function using list comprehension
-- length' xs = sum [1 | _ <- xs]

-- Playing with strings
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Playing with pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVENV!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n -1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName _   = "Bob"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' []    = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell []      = "The list is empty"
tell [x]     = "The list has one element: " ++ show x
tell [x,y]   = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) =
    "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- arguments as patterns. Keep the whole arg, while pattern matching it
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of '" ++ all ++ "' is " ++ [x]

-- guards and where exemple
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi    = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let <bindings> in <expression>
-- local scope, not visible to guards for ex.
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r^2
    in  sideArea + 2 * topArea

-- case, arguments pattern match is just a syntactic sugar for case
headCase :: [a] -> a
headCase xs = case xs of []    -> error "No head for empty list!"
                         (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what []  = "empty."
          what [x] = "a singleton list."
          what xs  = "a longer list."
