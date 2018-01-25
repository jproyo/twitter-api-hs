{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.Model (
TwitterError(..),
Tweet(text,userName,createdAt,retweetCount,favoriteCount),
UserTimeLine,
createTweet
) where

import           GHC.Generics                 (Generic)
import           Control.Applicative          ((<$>),(<*>),empty)
import           Data.Aeson                   (Value(..), FromJSON(..), ToJSON(..), (.:), (.=), object)
import           Data.Text                    (Text, pack)
import           Data.Time.Format             (parseTimeM, defaultTimeLocale)
import           Data.Time.Clock              (UTCTime)

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

instance ToJSON Tweet

type UserTimeLine = [Tweet]

data TwitterError = RequestError
                  | CredentialsError
                  | APIError
                  deriving (Show, Eq)

instance ToJSON TwitterError where
  toJSON val = object [ "error" .= String (pack $ show val) ]

parseDate :: String -> Maybe UTCTime
parseDate date = parseTimeM True defaultTimeLocale "%a %h %d %T +0000 %Y" date :: Maybe UTCTime

createTweet :: Text -> Text -> Maybe UTCTime -> Int -> Int -> Tweet
createTweet text userName createdAt retweetCount favoriteCount = Tweet{..}
