{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.CacheAdapter (
newHandle
) where

import Control.Monad.IO.Class  (liftIO)
import Control.Concurrent.MVar (newMVar, withMVar)
import Data.Cache              as C (lookup)
import Data.Text               (Text)
import Twitter.Config          (Config(..), twitterEncKey)
import Twitter.Model           (UserTimeLine,TwitterError,createError,credentialError,apiError,createTweet)
import Twitter.Adapter         (Handle(..), TwitterHandle, TimeLineRequest(..), TwitterResponse, execute)

readCache :: Config -> Text -> IO (Maybe UserTimeLine)
readCache config username = do
  cacheEng <- liftIO $ return $ cache config
  C.lookup cacheEng username

cacheTimeLine :: Config -> TimeLineRequest -> TwitterResponse
cacheTimeLine config req = do
  maybeTimeLine <- liftIO $ readCache config (userName req)
  liftIO $ putStrLn ("Cache Read: " ++ show maybeTimeLine)
  let maybeToEither (Just val) = Just (Right val)
      maybeToEither Nothing    = Nothing
      in return $ maybeToEither maybeTimeLine

newHandle :: Config -> IO TwitterHandle
newHandle config = do
    mutex <- newMVar ()

    return Handle
      { execute = \timelineReq ->
            withMVar mutex $ \() -> cacheTimeLine config timelineReq
      }
