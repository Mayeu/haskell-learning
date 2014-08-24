import Control.Applicative

--sequenceA :: (Applicative f) => [f a] -> f [a]
--sequenceA [] = pure []
--sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- hlint say "use foldr instead!"
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
