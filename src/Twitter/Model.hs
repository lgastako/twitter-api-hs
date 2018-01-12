{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.Model (
Tweet(text,userName,createdAt,retweetCount,favoriteCount)
) where

import           Control.Applicative
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Format
import           Data.Time.Clock
import           GHC.Generics

data Tweet = Tweet {
  text :: Text,
  userName :: Text,
  createdAt :: Maybe UTCTime,
  retweetCount :: Int,
  favoriteCount :: Int
} deriving (Generic, Show)

instance FromJSON Tweet where
    parseJSON (Object v) = do
      text <- v .: "text"
      userName <- (v .: "user") >>= (.: "screen_name")
      createdAtStr <- v .: "created_at"
      let createdAt = parseDate createdAtStr
      retweetCount <- v .: "retweet_count"
      favoriteCount <- v .: "favorite_count"
      return Tweet{..}
    parseJSON _          = empty

parseDate :: String -> Maybe UTCTime
parseDate date = parseTimeM True defaultTimeLocale "%a %h %d %T +0000 %Y" date :: Maybe UTCTime

instance ToJSON Tweet
