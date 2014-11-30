import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int
source = CL.sourceList [1..10]

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String
conduit = CL.map show

conduit2 :: Conduit Int IO Int
conduit2 = CL.map (*2)

main :: IO ()
main =
    -- source $$ conduit =$ sink
    -- source $= conduit $$ sink
    -- source $$ conduit2 =$= conduit =$ sink
    source $= conduit2 =$= conduit $$ sink
