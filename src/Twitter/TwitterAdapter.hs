{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Twitter.TwitterAdapter ( newHandle ) where

import           Control.Applicative        (empty, (<$>), (<*>))
import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Core.Utils                 (fromMaybeT, maybeToLeft)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), object, (.:), (.=))
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Conversion (toByteString')
import           Data.Either                (either)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import qualified Data.Text.Encoding         as E
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager, newManager,
                                             setQueryString)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Simple
import           Twitter.Adapter            (Handle (..), TimeLineRequest (..),
                                             TwitterResponse)
import           Twitter.Config             (twitterEncKey)
import           Twitter.Context            (Context, Logging (debug), conf)
import           Twitter.Model              (TwitterError, UserTimeLine,
                                             createError, credentialError)

data Token = Token { tokenType :: Text, accessToken :: Text }
    deriving (Generic, Show)

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "token_type" <*> v .: "access_token"
    parseJSON _          = empty

instance ToJSON Token where
    toJSON (Token tToken aToken) =
      object ["token_type" .= tToken, "access_token" .= aToken]

type ApiResponse a m = m (Either TwitterError a)
type TokenResponse m = ApiResponse Token m
type TimeLineResponse m = ApiResponse UserTimeLine m

extractResponse :: (FromJSON a) => Request -> ApiResponse a IO
extractResponse request = do
    response <- httpJSONEither request
    let onError  _     =
          Left
          $ fromJust (createError (fromIntegral $ getResponseStatusCode response))
        onSuccess resp =
            maybeToLeft
            resp
            (createError (fromIntegral $ getResponseStatusCode response))
    return $ either onError onSuccess (getResponseBody response)

requestBearer
  :: (MonadReader Context m, MonadIO m)
  => Manager
  -> TokenResponse m
requestBearer manager = do
    config <- asks conf
    fromMaybeT (return $ Left credentialError) $ do
        key <- MaybeT (return $ twitterEncKey config)
        liftIO $ do
            request <- parseRequest "https://api.twitter.com"
            let request'
                    = setRequestMethod "POST"
                    $ setRequestHeader "Authorization" [S8.concat ["Basic ", key]]
                    $ setRequestHeader "Content-Type"
                            ["application/x-www-form-urlencoded;charset=UTF-8"]
                    $ setRequestPath "/oauth2/token"
                    $ setRequestBodyLBS "grant_type=client_credentials"
                    $ setRequestSecure True
                    $ setRequestPort 443
                    $ setRequestManager manager request
            extractResponse request'


authorizationToken :: Token -> [S8.ByteString]
authorizationToken token =
  [ S8.concat
    [ E.encodeUtf8 (tokenType token)
    , " "
    , E.encodeUtf8 (accessToken token)
    ]
  ]

setParamRequestTimeLine
  :: TimeLineRequest
  -> [(S8.ByteString, Maybe S8.ByteString)]
setParamRequestTimeLine timelineReq =
    [ ("screen_name", Just (E.encodeUtf8 (userName timelineReq)))
    , ("count", Just (toByteString' $ fromMaybe 10 (limit timelineReq))) ]

requestUserTimeline :: (MonadIO m) =>
        Manager -> TimeLineRequest -> Token -> TimeLineResponse m
requestUserTimeline manager timelineReq token = liftIO $ do
    request <- parseRequest "https://api.twitter.com"
    let request'
            = setRequestMethod "GET"
            $ setRequestHeader "Authorization" (authorizationToken token)
            $ setRequestPath "/1.1/statuses/user_timeline.json"
            $ setQueryString (setParamRequestTimeLine timelineReq)
            $ setRequestSecure True
            $ setRequestPort 443
            $ setRequestManager manager request
    extractResponse request'

userTimeline
  :: (MonadReader Context m, MonadIO m)
  => TimeLineRequest
  -> TimeLineResponse m
userTimeline timelineReq = runExceptT $ do
    httpConnManager <- liftIO $ newManager tlsManagerSettings
    bearer <- ExceptT $ requestBearer httpConnManager
    ExceptT $ requestUserTimeline httpConnManager timelineReq bearer

searchAndCache
  :: (MonadReader Context m, MonadIO m, Logging m)
  => TimeLineRequest
  -> m TwitterResponse
searchAndCache timelineReq = do
    debug $ "Searching timeline in Twitter API for " <> pack (show timelineReq)
    result <- userTimeline timelineReq
    return (Just result)

newHandle :: (Logging m, MonadIO m) => Handle m
newHandle = Handle { timeline = searchAndCache }
