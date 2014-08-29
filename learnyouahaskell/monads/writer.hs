import Data.Monoid
import Control.Monad.Writer

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

type Food  = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

-- "applyLog' is what Writer do:
-- newtype Writer w a = Writer { runWriter :: (a, w) }
--
-- Defined as an instance of Monad
--
-- instance (Monoid w) => Monad (Writer w) where
--      return x = Writer (x, mempty)
--      (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- Now we have a monad, we can do notate
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got Number: " ++ show x])

mulWithLog :: Writer [String] Int
mulWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a * b)

-- Euclid's algorithm, with log
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- mappend can be really inefficient some time, here is an inefficient version
-- of logging for ou gcd

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

-- turning list in function, for fun, profit and efficiency
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList $ f . g

gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd'' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
