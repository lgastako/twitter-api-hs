module Twitter.CacheAdapter (
newHandle
) where

import Control.Concurrent.MVar ( newMVar
                               , withMVar
                               )
import Control.Monad.IO.Class  ( liftIO )
import Data.Text               ( Text )
import Twitter.Adapter         ( Handle(..)
                               , TimeLineRequest(..)
                               , TwitterHandle
                               , TwitterResponse
                               , execute
                               )
import Twitter.Config          ( Config
                               , readFromCache
                               )
import Twitter.Model           ( TwitterError
                               , UserTimeLine
                               , apiError
                               , createError
                               , createTweet
                               , credentialError
                               )

cacheTimeLine :: Config -> TimeLineRequest -> TwitterResponse
cacheTimeLine config req = do
  maybeTimeLine <- liftIO $ readFromCache config (userName req)
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
