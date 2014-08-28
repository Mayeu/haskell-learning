import Control.Monad

-- monad instance definition of list
--
-- instance Monad [] where
--      return x = [x]
--      xs >>= f = concat (map f xs)
--      fail _ = []

-- Return the combination of all tuples for two lists
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1, 2, 3]
    ch <- "abc"
    return (n, ch)

-- ^ it is basically list comprehension. Those are translated into monad.
--
-- So, how do you filter with monad like in list comprehension?
--
-- class Monad m => MonadPlus m where
--      mzero :: m a
--      mplus :: m a -> m a -> m a
--
-- mzero act as mempty from Monoid, and mplus as mappend. Since list are monad
-- and monoid, you can have:
--
-- instance MonadPlus [] where
--      mzero = []
--      mplus = (++)
--
-- With that we can have a guard function (defined in Control.Monad)
--
-- guard :: (MonadPlus m) => Bool -> m ()
-- guard True = return ()
-- guard False = mzero
--
-- Which can act as a filter!
--
-- ex: [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x
