{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import Yesod

data Piggies = Piggies

instance Yesod Piggies

mkYesod "Piggies" [parseRoutes|
  /      HomeR GET
  /about AboutR GET
|]

getHomeR = defaultLayout [whamlet|
  <h1>Welcome to the Pigsty!

  <p>
      If you'd like to know more
      <a href=@{AboutR}>about us.
      we'll be happy to oblige.
|]

getAboutR = defaultLayout [whamlet|
  Enough about us!
  <br>
  <a href=@{HomeR}>Back home.
|]

main = warp 3000 Piggies
