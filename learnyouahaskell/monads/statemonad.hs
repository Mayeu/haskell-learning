import Control.Monad.State

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x, xs)
-- 
-- push :: Int -> Stack -> ((), Stack)
-- push a xs = ((),a:xs)
-- 
-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack = let
--                        ((), newStack1) = push 3 stack
--                        (a,  newStack2) = pop newStack1
--                    in pop newStack2

-- State monad version

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
       then push 5
       else do
           push 3
           push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    Control.Monad.when (a == 100) stackStuff

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
       then put [8,3,1]
       else put [9,2,1]
