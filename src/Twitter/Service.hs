module Twitter.Service
  (
  TwitterService(..),
  getUserTimeline
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Twitter.Adapter           (Handle, TimeLineRequest,
                                            TwitterHandle,
                                            createTimeLineRequest, timeline)
import           Twitter.CacheAdapter      as CA
import           Twitter.Config            (Config)
import           Twitter.Model             (TwitterError, UserTimeLine)
import qualified Twitter.TwitterAdapter    as TA

class Monad m => TwitterService m where
  getTimeLine :: Config -> TimeLineRequest -> m (Either TwitterError UserTimeLine)

instance TwitterService IO where
  getTimeLine config request = do
    result     <- runMaybeT $  MaybeT (getFromCache config request)
                           <|> MaybeT (getFromTwitter config request)
    return $ fromJust result

getFromCache :: Config -> TimeLineRequest -> IO (Maybe (Either TwitterError UserTimeLine))
getFromCache config req = CA.newHandle config >>= flip timeline req

getFromTwitter :: Config -> TimeLineRequest -> IO (Maybe (Either TwitterError UserTimeLine))
getFromTwitter config req = TA.newHandle config >>= flip timeline req

getUserTimeline :: TwitterService m => Config -> Text -> Maybe Int -> m (Either TwitterError UserTimeLine)
getUserTimeline config userName limit = getTimeLine config (createTimeLineRequest userName limit)
