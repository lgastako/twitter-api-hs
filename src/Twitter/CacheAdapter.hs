module Twitter.CacheAdapter (
newHandle
) where

import           Control.Concurrent.MVar (newMVar, withMVar)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Cache              as C (lookup)
import           Data.Text               (Text)
import           Twitter.Adapter         (Handle (..), TimeLineRequest (..),
                                          TwitterHandle, TwitterResponse,
                                          execute)
import           Twitter.Config          (Config (..), twitterEncKey)
import           Twitter.Model           (TwitterError, UserTimeLine, apiError,
                                          createError, createTweet,
                                          credentialError)

readCache :: Config -> Text -> IO (Maybe UserTimeLine)
readCache config username = liftIO (return $ cache config) >>= flip C.lookup username

cacheTimeLine :: Config -> TimeLineRequest -> TwitterResponse
cacheTimeLine config req = do
  maybeTimeLine <- liftIO $ readCache config (userName req)
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
