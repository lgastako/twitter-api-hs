{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module App (runApp, app) where

import           Control.Monad.IO.Class
import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S
import           Twitter.Adapter
import           Twitter.Model

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.json $ object ["endpoints" .= object ["user_timeline" .= String "/user/{userName}/timeline"] ]

  S.get "/user/:userName/timeline" $ do
    userName <- S.param "userName"
    limit <- S.param "limit" `S.rescue` (\x -> return 10)
    timeLine <- liftIO $ userTimeline userName (Just limit)
    S.json (timeLine :: [Tweet])

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'
