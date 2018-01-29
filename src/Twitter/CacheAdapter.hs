{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.CacheAdapter (
newHandle
) where

import           Control.Concurrent.MVar    (newMVar, withMVar)
import           Twitter.Config             (Config, twitterEncKey)
import           Twitter.Model              (UserTimeLine,TwitterError,createError,credentialError,apiError,createTweet)
import           Twitter.Adapter            (Handle(..), TwitterHandle, TimeLineRequest(..), TwitterResponse, execute)

cacheTimeLine :: Config -> TimeLineRequest -> TwitterResponse
-- cacheTimeLine _ _ = return $ Just (Right [createTweet "some" "myuser" Nothing 20 1])
cacheTimeLine _ _ = return Nothing

newHandle :: Config -> IO TwitterHandle
newHandle config = do
    mutex <- newMVar ()

    return Handle
      { execute = \timelineReq ->
            withMVar mutex $ \() -> cacheTimeLine config timelineReq
      }
