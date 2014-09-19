import Data.Ratio
import Data.List (all)

-- Representing some facts from a list: [(3,0.5),(5,0.25),(9,0.25)]
-- 3 happen 50% of the time, 5 and 9 25%

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

-- List are functors, so we are
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

-- To be a monad we need >>= which basically flatten. So first, what is flatenning
-- in this case?
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concatMap multAll xs
    where multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs

-- Now we can monad
instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f  = flatten (fmap f m)
    fail _   = Prob []

-- Now, we shall launch coins

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin:: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])
