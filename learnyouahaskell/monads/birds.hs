type Birds = Int
type Pole  = (Birds, Birds)

-- Bird landing on the left part of the pole
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs (newLeft - right) < 4 = Just (newLeft, right)
    | otherwise                           = Nothing
  where newLeft = left + n

-- Bird landing on the right part of the pole
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - newRight) < 4 = Just (left, newRight)
    | otherwise                    = Nothing
  where newRight = right + n

-- banana make Paul to always fall
banana :: Pole -> Maybe Pole
banana _ = Nothing

-- caling a routine using the do notation for monad
routine :: Maybe Pole
routine = do let start = (0,0)
             first <- landLeft 2 start
             second <- landRight 2 first
             landLeft 1 second
