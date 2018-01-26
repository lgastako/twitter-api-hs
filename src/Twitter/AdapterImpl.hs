{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.AdapterImpl (
newHandle
) where

import           GHC.Generics               (Generic)
import           Control.Applicative        ((<$>), (<*>), empty)
import           Control.Concurrent.MVar    (newMVar, withMVar)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Maybe  (MaybeT(..))
import           Data.Aeson                 (FromJSON(..), ToJSON(..), (.:), (.=), object, Value(..))
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion (toByteString')
import           Data.Maybe                 (fromMaybe)
import           Data.Either                (fromLeft, either)
import qualified Data.Text.Encoding         as E
import           Data.Text                  (Text)
import           Network.HTTP.Simple
import           Network.HTTP.Client
import           Core.Utils                 (fromMaybeT,maybeToLeft)
import           Twitter.Config             (Config, twitterEncKey)
import           Twitter.Model              (UserTimeLine,TwitterError(..))
import           Twitter.Adapter            (Handle(..), TimeLineRequest(..), execute)

data Token = Token { tokenType :: Text, accessToken :: Text } deriving (Generic, Show)

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "token_type" <*> v .: "access_token"
    parseJSON _          = empty

instance ToJSON Token where
    toJSON (Token tokenType accessToken) = object ["token_type" .= tokenType, "access_token" .= accessToken]

type ServiceResponse = Handle TimeLineRequest TwitterError UserTimeLine

mapStatusCode :: Int -> Maybe TwitterError
mapStatusCode code | code < 400 = Nothing
mapStatusCode 400 = Just RequestError
mapStatusCode 404 = Just RequestError
mapStatusCode 401 = Just CredentialsError
mapStatusCode 403 = Just CredentialsError
mapStatusCode _   = Just APIError

extractResponse :: (FromJSON a) => Request -> IO (Either TwitterError a)
extractResponse request = do
  response <- httpJSONEither request
  let onError   _    = Left APIError
      onSuccess resp = maybeToLeft resp (mapStatusCode (getResponseStatusCode response))
      in return $ either onError onSuccess (getResponseBody response)          

requestBearer :: Config -> IO (Either TwitterError Token)
requestBearer config = do
    fromMaybeT (return $ Left CredentialsError) $ do
      key <- MaybeT (return $ twitterEncKey config)
      liftIO $ do
        request' <- parseRequest "https://api.twitter.com"
        let request
                = setRequestMethod "POST"
                $ setRequestHeader "Authorization" [S8.concat ["Basic ", key]]
                $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded;charset=UTF-8"]
                $ setRequestPath "/oauth2/token"
                $ setRequestBodyLBS "grant_type=client_credentials"
                $ setRequestSecure True
                $ setRequestPort 443 request'
        extractResponse request


requestUserTimeline :: TimeLineRequest -> Token -> IO (Either TwitterError UserTimeLine)
requestUserTimeline timelineReq token = do
    request' <- parseRequest "https://api.twitter.com"
    let request
            = setRequestMethod "GET"
            $ setRequestHeader "Authorization" [S8.concat ["Bearer ", E.encodeUtf8 (accessToken token)]]
            $ setRequestPath "/1.1/statuses/user_timeline.json"
            $ setQueryString [("screen_name", Just (E.encodeUtf8 (userName timelineReq))), ("count", Just (toByteString' $ fromMaybe 10 (limit timelineReq)))]
            $ setRequestSecure True
            $ setRequestPort 443 request'
    extractResponse request


userTimeline :: Config -> TimeLineRequest -> IO (Either TwitterError UserTimeLine)
userTimeline config timelineReq = do
  val <- requestBearer config
  let extractError   _      = return $ Left (fromLeft CredentialsError val)
      performRequest bearer = requestUserTimeline timelineReq bearer
      in either extractError performRequest val

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
