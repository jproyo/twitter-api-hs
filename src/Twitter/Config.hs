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
import           Control.Applicative        (Applicative,(<*>),liftA2)
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
  consumerKey :: Maybe String,
  consumerSecret :: Maybe String
}

getConfig :: IO Config
getConfig = do
  environment <- getEnvironment
  twitter <- getTwitterConf
  return Config{..}

concatKeySecret :: Config -> Maybe String
concatKeySecret conf = liftA2 (++) ((++) <$> key <*> (Just ":")) secret
  where twitterConf = twitter conf
        key = consumerKey twitterConf
        secret = consumerSecret twitterConf

twitterEncKey :: Config -> Maybe S8.ByteString
twitterEncKey conf = do
  key <- concatKeySecret conf
  return $ B.encode (toByteString' key)

getEnvironment :: IO Environment
getEnvironment = (maybe Development read) <$> (lookupEnv "TWITTER_ENV")

getTwitterConf :: IO TwitterConf
getTwitterConf = do
  consumerKey <- lookupEnv "TWITTER_CONSUMER_KEY"
  consumerSecret <- lookupEnv "TWITTER_CONSUMER_SECRET"
  return TwitterConf{..}
