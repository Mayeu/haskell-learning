data TrafficLight = Red | Yellow | Green

-- TrafficLight is an instance of Eq.
-- Since /= is defined in terms of ==, no need to define /=, == is sufficient,
-- haskell will do the rest
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
