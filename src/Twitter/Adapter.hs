{-# LANGUAGE RecordWildCards #-}
-- | This is an abstract interface for a simple Twitter service. It is intended to be
-- imported qualified as follows.
--
-- > import qualified Twitter.Service as Service
--
module Twitter.Adapter
  (
    Handle(..)
  , TwitterHandle
  , TimeLineRequest(userName,limit)
  , createTimeLineRequest
  , timeline
  ) where

import           Data.Text         (Text)
import           Twitter.Model     (UserTimeLine, TwitterError)

newtype Handle a e b = Handle { execute :: a -> IO (Either e b) }

type TwitterHandle = Handle TimeLineRequest TwitterError UserTimeLine

data TimeLineRequest = TimeLineRequest { userName :: Text, limit :: Maybe Int }

createTimeLineRequest :: Text -> Maybe Int -> TimeLineRequest
createTimeLineRequest userName limit = TimeLineRequest{..}

timeline :: TwitterHandle -> TimeLineRequest -> IO (Either TwitterError UserTimeLine)
timeline = execute
