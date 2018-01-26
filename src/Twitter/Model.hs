{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Twitter.Model (
TwitterError(..),
Tweet(text,userName,createdAt,retweetCount,favoriteCount),
UserTimeLine,
createTweet,
createError,
credentialError,
apiError
) where

import           GHC.Generics                 (Generic)
import           Control.Applicative          ((<$>),(<*>),empty)
import           Data.Aeson                   (Value(..), FromJSON(..), ToJSON(..), (.:), (.=), object)
import           Data.Data
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text, pack)
import           Data.Time.Format             (parseTimeM, defaultTimeLocale)
import           Data.Time.Clock              (UTCTime)
import           Data.Typeable

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

data TwitterError = RequestError { code :: Int }
                  | CredentialsError  { code :: Int }
                  | APIError  { code :: Int }
                  deriving (Typeable, Data)

instance Show TwitterError where
   show = showConstr . toConstr

instance ToJSON TwitterError where
  toJSON val = object [ "error" .= object [ "code" .= (code val), "message" .= String (pack $ show val)] ]

parseDate :: String -> Maybe UTCTime
parseDate date = parseTimeM True defaultTimeLocale "%a %h %d %T +0000 %Y" date :: Maybe UTCTime

createTweet :: Text -> Text -> Maybe UTCTime -> Int -> Int -> Tweet
createTweet text userName createdAt retweetCount favoriteCount = Tweet{..}

createError :: Int -> Maybe TwitterError
createError code | code < 400            = Nothing
                 | code `elem` [400,404] = Just RequestError{..}
                 | code `elem` [401,403] = Just CredentialsError{..}
                 | otherwise             = Just APIError{..}

credentialError :: TwitterError
credentialError = fromJust $ createError 401

apiError :: TwitterError
apiError = fromJust $ createError 500
