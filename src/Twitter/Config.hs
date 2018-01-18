{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Twitter.Config (
ConfigM(..),
Config(..),
Environment(..),
getConfig,
twitterEncKey
) where

import           Data.Aeson
import           System.Environment         (lookupEnv)
import           Control.Applicative        (Applicative)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Data.Maybe                 (maybe)
import qualified Data.ByteString.Base64     as B
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion


data Environment = Development
    | Production
    | Test
    deriving (Eq, Read, Show)

instance ToJSON Environment where
  toJSON e = object ["environment" .= show e]

data Config = Config
  { twitter :: TwitterConf
  , environment :: Environment
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

data TwitterConf = TwitterConf {
  consumerKey :: String,
  consumerSecret :: String
}

getConfig :: IO Config
getConfig = do
  environment <- getEnvironment
  twitter <- getTwitterConf
  return Config{..}

twitterEncKey :: Config -> S8.ByteString
twitterEncKey conf = B.encode byteStr
    where byteStr = toByteString' concatedKey
          concatedKey = key ++ ":" ++ secret
          twitterConf = twitter conf
          key = consumerKey twitterConf
          secret = consumerSecret twitterConf

lookOrDefault :: String -> String -> IO String
lookOrDefault key def = fmap (maybe def id) (lookupEnv key)

getEnvironment :: IO Environment
getEnvironment = fmap read $ lookOrDefault "TWITTER_ENV" "Development"

getTwitterConf :: IO TwitterConf
getTwitterConf = do
  consumerKey <- lookOrDefault "TWITTER_CONSUMER_KEY" ""
  consumerSecret <- lookOrDefault "TWITTER_CONSUMER_SECRET" ""
  return TwitterConf{..}
