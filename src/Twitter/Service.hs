module Twitter.Service
  (
  TwitterService(..),
  getUserTimeline
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad.Trans.Maybe  (MaybeT(..))
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Twitter.Config             (Config)
import           Twitter.Model              (UserTimeLine,TwitterError)
import           Twitter.Adapter            (Handle, TimeLineRequest, createTimeLineRequest, timeline)
import qualified Twitter.TwitterAdapter     as TA
import           Twitter.CacheAdapter       as CA

class Monad m => TwitterService m where
  getTimeLine :: Config -> TimeLineRequest -> m (Either TwitterError UserTimeLine)

instance TwitterService IO where
  getTimeLine config request = do
    twitterApi <- TA.newHandle config
    cache      <- CA.newHandle config
    result     <- runMaybeT $  MaybeT (timeline cache request)
                           <|> MaybeT (timeline twitterApi request)
    return $ fromJust result

getUserTimeline :: TwitterService m => Config -> Text -> Maybe Int -> m (Either TwitterError UserTimeLine)
getUserTimeline config userName limit = getTimeLine config (createTimeLineRequest userName limit)
