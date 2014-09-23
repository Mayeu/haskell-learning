data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- A free tree!
freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

-- Now, I want to change the W in P in this tree.
-- One solution is to make a direction structure

data Direction = L | R deriving (Show)
type Directions = [Direction]

-- Change something to P
changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

-- parcour a tree and say what is the destination
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

-- All this is cool, but that can be really inefficient. It would be good to be
-- able to focus on a subtree, and being able to move around easily

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
                          
goLeft' :: Zipper a -> Maybe (Zipper a)
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft' (Empty, _)       = Nothing

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goRight' :: Zipper a -> Maybe (Zipper a)
goRight' (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight' (Empty, _)       = Nothing

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r:bs)  = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, [])                = Nothing

-- Move around easily
x -: f = f x

-- A pair that contains a focused part of a data, and it suroundings
-- structure is called a zipper.

type Zipper a = (Tree a, Breadcrumbs a)

-- Modify the element currently focused on
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs)      = (Empty, bs)

-- Attach new subtree to a leaf
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- Go to the topMost item
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)
