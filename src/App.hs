{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (runApp, app) where

import           Data.Text.Lazy                       (Text)
import           Data.Default                         (def)
import           Data.Aeson                           (Value(..), object, (.=))
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader                 (runReaderT, asks, lift)
import           Control.Monad.Reader.Class           (ask)
import           Network.HTTP.Types.Status            (created201, internalServerError500, notFound404)
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.Warp             (Settings, defaultSettings, setFdCacheDuration, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Web.Scotty.Trans                     (ActionT, Options, ScottyT, scottyAppT, defaultHandler, get, json, rescue, middleware, notFound, param, scottyOptsT, settings, showError, status, verbose)
import           Twitter.Config                       (ConfigM, Config(..), Environment(..), runConfigM, getConfig, twitterEncKey)
import           Twitter.Model                        (UserTimeLine)
import           Twitter.Service                      (timeline, createTimeLineRequest)
import           Twitter.ServiceImpl                  (newHandle)


type Error = Text
type Action = ActionT Error ConfigM ()

getSettings :: Environment -> IO Settings
getSettings e = do
   let s = defaultSettings
       s' = case e of
         Development -> setFdCacheDuration 0 s
         Production -> s
         Test -> s
       s'' = setPort 8080 s'
   return s''

getOptions :: Environment -> IO Options
getOptions e = do
   s <- getSettings e
   return def
     { settings = s
     , verbose = case e of
       Development -> 1
       Production -> 0
       Test -> 0
     }

defaultH :: Error -> Action
defaultH x = do
  e <- lift $ asks environment
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production  = logStdout
loggingM _           = id

runConfig :: Config -> ConfigM a -> IO a
runConfig config m = runReaderT (runConfigM m) config

app :: IO Application
app = do
  config <- getConfig
  scottyAppT (runConfig config) (application (environment config))

runApp :: IO ()
runApp = getConfig >>= runApplication

runApplication :: Config -> IO ()
runApplication config = do
  o <- getOptions (environment config)
  scottyOptsT o (runConfig config) (application (environment config))

application :: Environment -> ScottyT Error ConfigM ()
application e = do
  middleware (loggingM e)
  defaultHandler defaultH
  get "/" rootAction
  get "/user/:userName/timeline" userTimelineAction
  notFound notFoundA

rootAction :: Action
rootAction = json $ object ["endpoints" .= object ["user_timeline" .= String "/user/{userName}/timeline"] ]

userTimelineAction :: Action
userTimelineAction = do
  config <- lift ask
  userName <- param "userName"
  limit :: Int <- param "limit" `rescue` (\x -> return 10)
  timeline <- liftIO $ do
    service <- newHandle config
    timeline service (createTimeLineRequest userName (Just limit))
  json (timeline :: UserTimeLine)

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null
