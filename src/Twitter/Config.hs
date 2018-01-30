{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Twitter.Config
  ( ConfigM(..)
  , Config(..)
  , Environment(..)
  , getConfig
  , twitterEncKey
  , readFromCache
  , putInCache
  ) where

import           Control.Applicative              ( (<*>)
                                                  , Applicative
                                                  , liftA2
                                                  )
import           Control.Monad.IO.Class           ( MonadIO )
import           Control.Monad.Reader             ( MonadReader
                                                  , ReaderT
                                                  )
import           Control.Monad.Trans.Class        ( MonadTrans )
import           Data.Aeson                       ( (.=)
                                                  , ToJSON
                                                  , object
                                                  , toJSON
                                                  )
import qualified Data.ByteString.Base64     as B
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion
import           Data.Cache                 as C  ( Cache
                                                  , insert
                                                  , lookup
                                                  , newCache
                                                  )
import           Data.Maybe                       ( maybe )
import           Data.Text                        ( Text )
import           System.Clock                     ( fromNanoSecs )
import           System.Environment               ( lookupEnv )
import           Twitter.Model                    ( UserTimeLine )

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

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

data TwitterConf = TwitterConf
  { consumerKey    :: Maybe String
  , consumerSecret :: Maybe String
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
concatKeySecret conf = liftA2 (++) ((++) <$> key <*> Just ":") secret
  where
    twitterConf = twitter conf
    key         = consumerKey twitterConf
    secret      = consumerSecret twitterConf

twitterEncKey :: Config -> Maybe S8.ByteString
twitterEncKey conf = B.encode . toByteString' <$> concatKeySecret conf

getEnvironment :: IO Environment
getEnvironment = maybe Development read <$> lookupEnv "TWITTER_ENV"

getTwitterConf :: IO TwitterConf
getTwitterConf = do
  consumerKey    <- lookupEnv "TWITTER_CONSUMER_KEY"
  consumerSecret <- lookupEnv "TWITTER_CONSUMER_SECRET"
  return TwitterConf{..}
