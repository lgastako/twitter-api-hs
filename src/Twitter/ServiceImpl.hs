{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Twitter.ServiceImpl (
newHandle
) where

import           GHC.Generics               (Generic)
import           Control.Applicative        ((<$>), (<*>), empty)
import           Control.Concurrent.MVar    (newMVar, withMVar)
import           Data.Aeson                 (FromJSON(..), ToJSON(..), (.:), (.=), object, Value(..))
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion (toByteString')
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.Encoding         as E
import           Data.Text                  (Text)
import           Network.HTTP.Simple
import           Network.HTTP.Client
import           Twitter.Config             (Config, twitterEncKey)
import           Twitter.Model              (UserTimeLine)
import           Twitter.Service            (Handle(..), TimeLineRequest(..), execute)

data Token = Token { tokenType :: Text, accessToken :: Text } deriving (Generic, Show)

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "token_type" <*> v .: "access_token"
    parseJSON _          = empty

instance ToJSON Token where
    toJSON (Token tokenType accessToken) = object ["token_type" .= tokenType, "access_token" .= accessToken]

type ServiceResponse = Handle TimeLineRequest UserTimeLine

consumerEnc :: Config -> IO S8.ByteString
consumerEnc config = return $ twitterEncKey config

bearer :: Config -> IO Token
bearer config = do
    token <- consumerEnc config
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

userTimeline :: Config -> TimeLineRequest -> IO UserTimeLine
userTimeline config timelineReq = do
    token <- bearer config
    request' <- parseRequest "https://api.twitter.com"
    let request
            = setRequestMethod "GET"
            $ setRequestHeader "Authorization" [S8.concat ["Bearer ", E.encodeUtf8 (accessToken token)]]
            $ setRequestPath "/1.1/statuses/user_timeline.json"
            $ setQueryString [("screen_name", Just (E.encodeUtf8 (userName timelineReq))), ("count", Just (toByteString' $ fromMaybe 10 (limit timelineReq)))]
            $ setRequestSecure True
            $ setRequestPort 443 request'
    response <- httpJSON request
    return (getResponseBody response :: UserTimeLine)


-- | Create a new 'Service.Handle' that calls to twitter api.
newHandle :: Config -> IO ServiceResponse
newHandle config = do
    -- We use a mutex to make our logger thread-safe.
    -- (Note that we should take this mutex as an argument for maximal
    -- compositionality.)
    mutex <- newMVar ()

    return Handle
      { execute = \timelineReq ->
            withMVar mutex $ \() -> userTimeline config timelineReq
      }
