{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Twitter.Adapter (
userTimeline
) where

import           GHC.Generics
import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion
import           Data.Time.Format
import           Data.Time.Clock
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding         as E
import           Data.Text (Text)
import           Network.HTTP.Simple
import           Network.HTTP.Client
import           Twitter.Config

data Token = Token { tokenType :: Text, accessToken :: Text } deriving (Generic, Show)

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "token_type" <*> v .: "access_token"
    parseJSON _          = empty

instance ToJSON Token where
    toJSON (Token tokenType accessToken) = object ["token_type" .= tokenType, "access_token" .= accessToken]

consumerEnc :: IO S8.ByteString
consumerEnc = do
  c <- config
  return (twitterEncKey c)

bearer :: IO Token
bearer = do
    token <- consumerEnc
    request' <- parseRequest "https://api.twitter.com"
    let request
            = setRequestMethod "POST"
            $ setRequestHeader "Authorization" [S8.concat ["Basic ", token]]
            $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded;charset=UTF-8"]
            $ setRequestPath "/oauth2/token"
            $ setRequestBodyLBS "grant_type=client_credentials"
            $ setRequestSecure True
            $ setRequestPort 443 request'
    response <- httpJSON request
    return (getResponseBody response :: Token)

data TweeterTimeLine = TweeterTimeLine {
  text :: Text,
  userName :: Text,
  createdAt :: Maybe UTCTime,
  retweetCount :: Int,
  favoriteCount :: Int
} deriving (Generic, Show)

instance FromJSON TweeterTimeLine where
    parseJSON (Object v) = do
      text <- v .: "text"
      userName <- (v .: "user") >>= (.: "screen_name")
      createdAtUnparsed <- (v .: "created_at")
      retweetCount <- v .: "retweet_count"
      favoriteCount <- v .: "favorite_count"
      return TweeterTimeLine{text = text, userName = userName, createdAt = parseDate createdAtUnparsed, retweetCount = retweetCount, favoriteCount = favoriteCount}
    parseJSON _          = empty

parseDate :: String -> Maybe UTCTime
parseDate date = parseTimeM True defaultTimeLocale "%a %h %d %T +0000 %Y" date :: Maybe UTCTime

userTimeline :: S8.ByteString -> Maybe Int -> IO [TweeterTimeLine]
userTimeline name limit = do
    token <- bearer
    request' <- parseRequest "https://api.twitter.com"
    let request
            = setRequestMethod "GET"
            $ setRequestHeader "Authorization" [S8.concat ["Bearer ", E.encodeUtf8 (accessToken token)]]
            $ setRequestPath "/1.1/statuses/user_timeline.json"
            $ setQueryString [("screen_name", Just name), ("count", Just (toByteString' $ fromMaybe 10 limit))]
            $ setRequestSecure True
            $ setRequestPort 443 request'
    response <- httpJSON request
    return (getResponseBody response :: [TweeterTimeLine])
