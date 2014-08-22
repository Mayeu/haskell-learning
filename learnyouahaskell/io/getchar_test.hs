main = do
    c <- getChar
    if c /= ' '
       then do
           putChar c
           main
       else do
           putChar '\n'
           return ()
