{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Twitter.Config (
Config(..),
Environment(..),
getConfig,
twitterEncKey,
readFromCache,
putInCache
) where

import           Data.Aeson                 (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString.Base64     as B
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion
import           Data.Cache                 as C (Cache, insert, lookup,
                                                  newCache)
import           Data.Maybe                 (maybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           System.Clock               (fromNanoSecs)
import           System.Environment         (lookupEnv)
import           Twitter.Model              (UserTimeLine)


data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

instance ToJSON Environment where
  toJSON e = object [ "environment" .= show e ]

data Config = Config
  { twitter     :: TwitterConf
  , environment :: Environment
  , cache       :: C.Cache Text UserTimeLine
  }

data TwitterConf = TwitterConf {
  consumerKey    :: Maybe String,
  consumerSecret :: Maybe String
}

readFromCache :: Config -> Text -> IO (Maybe UserTimeLine)
readFromCache config = C.lookup (cache config)

putInCache :: Config -> Text -> UserTimeLine -> IO ()
putInCache config = C.insert (cache config)

getConfig :: IO Config
getConfig = do
  environment <- getEnvironment
  twitter     <- getTwitterConf
  cache       <- C.newCache (Just (fromNanoSecs 30000000000))
  return Config{..}

concatKeySecret :: Config -> Maybe String
concatKeySecret conf = key conf <> Just ":" <> secret conf
  where key = consumerKey . twitter
        secret = consumerSecret . twitter

twitterEncKey :: Config -> Maybe S8.ByteString
twitterEncKey conf = B.encode . toByteString' <$> concatKeySecret conf

getEnvironment :: IO Environment
getEnvironment = maybe Development read <$> lookupEnv "TWITTER_ENV"

getTwitterConf :: IO TwitterConf
getTwitterConf = do
  consumerKey    <- lookupEnv "TWITTER_CONSUMER_KEY"
  consumerSecret <- lookupEnv "TWITTER_CONSUMER_SECRET"
  return TwitterConf{..}
