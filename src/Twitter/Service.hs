-- | This is an abstract interface for a simple Twitter service. It is intended to be
-- imported qualified as follows.
--
-- > import qualified Twitter.Service as Service
--
module Twitter.Service
  ( -- * Abstract handles
    Handle(..)
  , userTimeline
  ) where

import qualified Data.Text as T

newtype Handle a = Handle { execute :: T.Text -> Maybe Int -> IO a }

userTimeline :: Handle a -> T.Text -> Maybe Int -> IO a
userTimeline = execute
