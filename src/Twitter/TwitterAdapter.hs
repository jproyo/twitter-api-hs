{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Twitter.TwitterAdapter (
newHandle
) where

import           Control.Applicative        (empty, (<$>), (<*>))
import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks, lift,
                                             runReaderT)
import           Control.Monad.Reader.Class (ask)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Core.Utils                 (fromMaybeT, maybeToLeft)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), object, (.:), (.=))
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion (toByteString')
import           Data.Either                (either)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as E
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager, newManager,
                                             setQueryString)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Simple
import           Twitter.Adapter            (Handle (..), TimeLineRequest (..),
                                             TwitterResponse)
import           Twitter.Config             (Config, twitterEncKey)
import           Twitter.Context            (Context, conf, putInCache)
import           Twitter.Model              (TwitterError, UserTimeLine,
                                             apiError, createError,
                                             credentialError)

data Token = Token { tokenType :: Text, accessToken :: Text } deriving (Generic, Show)

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "token_type" <*> v .: "access_token"
    parseJSON _          = empty

instance ToJSON Token where
    toJSON (Token tokenType accessToken) = object ["token_type" .= tokenType, "access_token" .= accessToken]

type ApiResponse a m = m (Either TwitterError a)
type TokenResponse m = ApiResponse Token m
type TimeLineResponse m = ApiResponse UserTimeLine m

extractResponse :: (FromJSON a) => Request -> ApiResponse a IO
extractResponse request = do
  response <- httpJSONEither request
  let onError   _    = Left $ fromJust (createError (getResponseStatusCode response))
      onSuccess resp = maybeToLeft resp (createError (getResponseStatusCode response))
  return $ either onError onSuccess (getResponseBody response)

requestBearer :: (MonadReader Context m, MonadIO m) => Manager -> TokenResponse m
requestBearer manager = do
    config <- asks conf
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
                $ setRequestPort 443
                $ setRequestManager manager request
        extractResponse request'


requestUserTimeline :: (MonadReader Context m, MonadIO m) => Manager -> TimeLineRequest -> Token -> TimeLineResponse m
requestUserTimeline manager timelineReq token = liftIO $ do
  request <- parseRequest "https://api.twitter.com"
  let request'
          = setRequestMethod "GET"
          $ setRequestHeader "Authorization" [S8.concat ["Bearer ", E.encodeUtf8 (accessToken token)]]
          $ setRequestPath "/1.1/statuses/user_timeline.json"
          $ setQueryString [("screen_name", Just (E.encodeUtf8 (userName timelineReq))), ("count", Just (toByteString' $ fromMaybe 10 (limit timelineReq)))]
          $ setRequestSecure True
          $ setRequestPort 443
          $ setRequestManager manager request
  extractResponse request'

cacheResult :: (MonadReader Context m, MonadIO m) => Text -> Either TwitterError UserTimeLine -> m ()
cacheResult username timeline = do
  cxt <- ask
  liftIO $ either (\_ -> return ()) (putInCache cxt username) timeline

userTimeline :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> TimeLineResponse m
userTimeline timelineReq = runExceptT $ do
  httpConnManager <- liftIO $ newManager tlsManagerSettings
  bearer <- ExceptT $ requestBearer httpConnManager
  ExceptT $ requestUserTimeline httpConnManager timelineReq bearer

searchAndCache :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m TwitterResponse
searchAndCache timelineReq = do
  timeline <- userTimeline timelineReq
  cacheResult (userName timelineReq) timeline
  return (Just timeline)

newHandle :: (MonadReader Context m, MonadIO m) => Handle m
newHandle = Handle { timeline = searchAndCache }
