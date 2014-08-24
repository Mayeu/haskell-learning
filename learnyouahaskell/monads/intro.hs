-- Monad is the natural extension of Applicative
-- Defined like (>>=) :: (Monad m) => m a -> (a -> m b) -> mb
--
-- Exemple: defining what will become >>= for Maybe
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

-- Monad definition:
--
-- class Monad m where
--     return :: a -> m a
--
--     (>>=) :: m a -> (a -> m b) -> m b
--
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--
--     fail :: String -> m a
--     fail msg = error msg

-- Maybe as instance of Monad:
--
-- instance Monad Maybe where
--     return x      = Just x
--     Nothing >>= f = Nothing
--     Just x >>= f  = f x
--     fail _        = Nothing
