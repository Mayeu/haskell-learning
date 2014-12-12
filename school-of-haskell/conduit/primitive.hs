import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class

source :: Source IO Int
source = do
    yield 1
    yield 2
    yield 3
    yield 4

sourceList :: Monad m => [a] -> Source m a
sourceList = mapM_ yield

conduit :: Conduit Int IO String
conduit = do
    -- Get all the dajacent pairs from the stream
    mi1 <- await
    mi2 <- await
    case (mi1, mi2) of
        (Just i1, Just i2) -> do
            yield $ show (i1, i2)
            leftover i2
            conduit
        _ -> return ()

--sink :: Sink String IO ()
--sink = do
--    mstr <- await
--    case mstr of
--        Nothing -> return ()
--        Just str -> do
--            liftIO $ putStrLn str
--            sink

-- Better sink that just wait forever
sink :: Sink String IO ()
sink = awaitForever $ liftIO . putStrLn

myAwaitForever :: Monad m => (a -> Conduit a m b) -> Conduit a m b
myAwaitForever f =
    await >>= maybe (return ()) (\x -> f x >> myAwaitForever f)

triple :: Monad m => Conduit a m a
triple = awaitForever $ CL.sourceList . replicate 3

multiply :: Monad m => Conduit Int m Int
multiply = do
    ma <- await
    case ma of
        Nothing -> return ()
        Just a  -> CL.map (* a)

main :: IO ()
--main = source $$ conduit =$ sink
--main = sourceList [1,2,3,4,5] $$ conduit =$ sink
main = CL.sourceList [5..10] $$ multiply =$ CL.mapM_ print
