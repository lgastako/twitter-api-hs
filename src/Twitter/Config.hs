{-# LANGUAGE OverloadedStrings #-}
module Twitter.Config (
config,
twitterEncKey
) where

import           System.Environment
import           Data.Maybe
import qualified Data.ByteString.Base64     as B
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion


newtype Config = Config { twitter :: TwitterConf }

data TwitterConf = TwitterConf {
  consumerKey :: String,
  consumerSecret :: String
}

config :: IO Config
config = do
  twitterConf <- getTwitterConf
  return Config { twitter = twitterConf }

twitterEncKey :: Config -> S8.ByteString
twitterEncKey conf = B.encode byteStr
    where byteStr = toByteString' concatedKey
          concatedKey = key ++ ":" ++ secret
          twitterConf = twitter conf
          key = consumerKey twitterConf
          secret = consumerSecret twitterConf

getTwitterConf :: IO TwitterConf
getTwitterConf = do
  key <- lookupEnv "TWITTER_CONSUMER_KEY"
  secret <- lookupEnv "TWITTER_CONSUMER_SECRET"
  return TwitterConf { consumerKey = fromMaybe "" key, consumerSecret = fromMaybe "" secret }
