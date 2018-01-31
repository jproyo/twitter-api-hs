{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitter.TwitterAdapter (
newHandle
) where

import           Control.Applicative        (empty, (<$>), (<*>))
import           Control.Concurrent.MVar    (newMVar, withMVar)
import           Control.Monad              (mplus)
import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Core.Utils                 (fromMaybeT, maybeToLeft)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), object, (.:), (.=))
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion (toByteString')
import           Data.Cache                 as C (insert)
import           Data.Either                (either, fromLeft)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as E
import           GHC.Generics               (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Twitter.Adapter            (Handle (..), TimeLineRequest (..),
                                             TwitterHandle, TwitterResponse,
                                             execute)
import           Twitter.Config             (Config (..), putInCache,
                                             twitterEncKey)
import           Twitter.Model              (TwitterError, UserTimeLine,
                                             apiError, createError,
                                             credentialError)

data Token = Token { tokenType :: Text, accessToken :: Text } deriving (Generic, Show)

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "token_type" <*> v .: "access_token"
    parseJSON _          = empty

instance ToJSON Token where
    toJSON (Token tokenType accessToken) = object ["token_type" .= tokenType, "access_token" .= accessToken]

type ApiResponse a = IO (Either TwitterError a)
type TokenResponse = ApiResponse Token
type TimeLineResponse = ApiResponse UserTimeLine

extractResponse :: (FromJSON a) => Request -> ApiResponse a
extractResponse request = do
  response <- httpJSONEither request
  let onError   _    = Left $ fromJust (createError (getResponseStatusCode response))
      onSuccess resp = maybeToLeft resp (createError (getResponseStatusCode response))
  return $ either onError onSuccess (getResponseBody response)

requestBearer :: Config -> TokenResponse
requestBearer config =
    fromMaybeT (return $ Left credentialError) $ do
      key <- MaybeT (return $ twitterEncKey config)
      liftIO $ do
        request <- parseRequest "https://api.twitter.com"
        let request'
                = setRequestMethod "POST"
                $ setRequestHeader "Authorization" [S8.concat ["Basic ", key]]
                $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded;charset=UTF-8"]
                $ setRequestPath "/oauth2/token"
                $ setRequestBodyLBS "grant_type=client_credentials"
                $ setRequestSecure True
                $ setRequestPort 443 request
        extractResponse request'


requestUserTimeline :: TimeLineRequest -> Token -> TimeLineResponse
requestUserTimeline timelineReq token = do
    request <- parseRequest "https://api.twitter.com"
    let request'
            = setRequestMethod "GET"
            $ setRequestHeader "Authorization" [S8.concat ["Bearer ", E.encodeUtf8 (accessToken token)]]
            $ setRequestPath "/1.1/statuses/user_timeline.json"
            $ setQueryString [("screen_name", Just (E.encodeUtf8 (userName timelineReq))), ("count", Just (toByteString' $ fromMaybe 10 (limit timelineReq)))]
            $ setRequestSecure True
            $ setRequestPort 443 request
    extractResponse request'

cacheResult :: Config -> Text -> Either TwitterError UserTimeLine -> IO ()
cacheResult config username = either (\_ -> return ()) (putInCache config username)

userTimeline :: Config -> TimeLineRequest -> TimeLineResponse
userTimeline config timelineReq = runExceptT $ do
  bearer <- ExceptT $ requestBearer config
  ExceptT $ requestUserTimeline timelineReq bearer

-- | Create a new 'Twitter.Adapter.Handle' that calls to twitter api.
newHandle :: Config -> IO TwitterHandle
newHandle config = do
  mutex <- newMVar ()
  return Handle
    { execute = \timelineReq ->
          withMVar mutex $ \() -> do
            timeline <- userTimeline config timelineReq
            liftIO $ cacheResult config (userName timelineReq) timeline
            return $ Just timeline
    }
