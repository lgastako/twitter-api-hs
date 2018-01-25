module Twitter.Service
  (
  TwitterService(..),
  getUserTimeline
  ) where

import           Data.Text           (Text)
import           Twitter.Config      (Config)
import           Twitter.Model       (UserTimeLine,TwitterError)
import           Twitter.Adapter     (Handle, TimeLineRequest, createTimeLineRequest, timeline)
import           Twitter.AdapterImpl (newHandle)

class Monad m => TwitterService m where
  getTimeLine :: Config -> TimeLineRequest -> m (Either TwitterError UserTimeLine)

instance TwitterService IO where
  getTimeLine config request = do
    service <- newHandle config
    timeline service request

getUserTimeline :: TwitterService m => Config -> Text -> Maybe Int -> m (Either TwitterError UserTimeLine)
getUserTimeline config userName limit = getTimeLine config (createTimeLineRequest userName limit)
