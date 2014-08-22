import System.IO

-- first way
--main = do
--    handle <- openFile "girlfriend.txt" ReadMode
--    contents <- hGetContents handle
--    putStr contents
--    hClose handle

-- second way
--main = do
--    withFile' "girlfriend.txt" ReadMode (\handle -> do
--        contents <- hGetContents handle
--        putStr contents)
--
---- third way, we can implement our own withFile if we want
--withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
--withFile' path mode f = do
--    handle <- openFile path mode
--    result <- f handle
--    hClose handle
--    return result

-- forth way
main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
