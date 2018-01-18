{-# LANGUAGE RecordWildCards #-}
-- | This is an abstract interface for a simple Twitter service. It is intended to be
-- imported qualified as follows.
--
-- > import qualified Twitter.Service as Service
--
module Twitter.Service
  (
    Handle(..)
  , TimeLineRequest(userName,limit)
  , createTimeLineRequest
  , timeline
  ) where

import           Data.Text         (Text)
import           Twitter.Model     (UserTimeLine)

newtype Handle a b = Handle { execute :: a -> IO b }

data TimeLineRequest = TimeLineRequest { userName :: Text, limit :: Maybe Int }

createTimeLineRequest :: Text -> Maybe Int -> TimeLineRequest
createTimeLineRequest userName limit = TimeLineRequest{..}

timeline :: Handle TimeLineRequest UserTimeLine -> TimeLineRequest -> IO UserTimeLine
timeline = execute
