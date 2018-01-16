{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Twitter.ServiceImpl (
newHandle
) where

import qualified Twitter.Service            as Service
import           GHC.Generics
import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding         as E
import           Data.Text (Text)
import           Network.HTTP.Simple
import           Network.HTTP.Client
import           Twitter.Config
import           Twitter.Model
import           Control.Concurrent.MVar (newMVar, withMVar)

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

userTimeline :: Text -> Maybe Int -> IO [Tweet]
userTimeline name limit = do
    token <- bearer
    request' <- parseRequest "https://api.twitter.com"
    let request
            = setRequestMethod "GET"
            $ setRequestHeader "Authorization" [S8.concat ["Bearer ", E.encodeUtf8 (accessToken token)]]
            $ setRequestPath "/1.1/statuses/user_timeline.json"
            $ setQueryString [("screen_name", Just (E.encodeUtf8 name)), ("count", Just (toByteString' $ fromMaybe 10 limit))]
            $ setRequestSecure True
            $ setRequestPort 443 request'
    response <- httpJSON request
    return (getResponseBody response :: [Tweet])

type ServiceResponse = Service.Handle [Tweet]

-- | Create a new 'Service.Handle' that calls to twitter api.
newHandle :: IO ServiceResponse
newHandle = do
    -- We use a mutex to make our logger thread-safe.
    -- (Note that we should take this mutex as an argument for maximal
    -- compositionality.)
    mutex <- newMVar ()

    return Service.Handle
      { Service.execute = \userName limit ->
            withMVar mutex $ \() -> userTimeline userName limit
      }
