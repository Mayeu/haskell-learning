import Data.Monoid

-- Monoid have the following propriety:
--  * it is associative (do not confond with commutative)
--  * it have an identity element
--  (see semi-group theory)

-- Ordering is a monoid. Given this function that alphabetically compare strings
-- only if they are of equals length, otherwise return the length comparison:
--
--lengthCompare :: String -> String -> Ordering
--lengthCompare x y = let a = length x `compare` length y
--                        b = x `compare` y
--                    in  if a == EQ then b else a
--
-- We can rewrite it like:
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
-- mappend will always keep the left param, unless it is EQ
