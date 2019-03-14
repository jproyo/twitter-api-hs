{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Twitter.Model (
  TwitterError(..),
  Tweet(..),
  UserTimeLine,
  createTweet,
  createError,
  credentialError,
  apiError
) where

import           Control.Applicative (empty)
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, (.:), (.=))
import           Data.Maybe          (fromJust)
import           Data.Text           (Text, pack)
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    (defaultTimeLocale, parseTimeM)
import           GHC.Generics        (Generic)

data Tweet = Tweet
  { text          :: Text
  , userName      :: Text
  , createdAt     :: Maybe UTCTime
  , retweetCount  :: Integer
  , favoriteCount :: Integer
  } deriving (Generic, Show, Eq)

instance FromJSON Tweet where
    parseJSON (Object v) = do
        text          <- v .: "text"
        userName      <- (v .: "user") >>= (.: "screen_name")
        createdAtStr  <- v .: "created_at"
        let createdAt  = parseDate createdAtStr
        retweetCount  <- v .: "retweet_count"
        favoriteCount <- v .: "favorite_count"
        return Tweet{..}
    parseJSON _          = empty

instance ToJSON Tweet

type UserTimeLine = [Tweet]

data Error
  = RequestError
  | CredentialsError
  | APIError
  deriving (Show, Eq)

data TwitterError
  = TwitterError Error Integer
  deriving (Show, Eq)

instance ToJSON TwitterError where
    toJSON (TwitterError err code) =
      object
      [ "error" .=
        object [ "code" .= code, "message" .= String (pack $ show err)]
      ]

parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%a %h %d %T +0000 %Y"

createTweet :: Text -> Text -> Maybe UTCTime -> Integer -> Integer -> Tweet
createTweet = Tweet

createError :: Integer -> Maybe TwitterError
createError code | code < 400            = Nothing
                 | code `elem` [400,404] = Just $ TwitterError RequestError code
                 | code `elem` [401,403] = Just $ TwitterError CredentialsError code
                 | otherwise             = Just $ TwitterError APIError code

credentialError :: TwitterError
credentialError = fromJust $ createError 401

apiError :: TwitterError
apiError = fromJust $ createError 500
