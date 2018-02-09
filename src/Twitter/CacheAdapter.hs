{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Twitter.CacheAdapter (
newHandle
) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks, lift,
                                             runReaderT)
import           Control.Monad.Reader.Class (ask)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, unpack)
import           Twitter.Adapter            (Handle (..), TimeLineRequest (..),
                                             TwitterResponse)
import           Twitter.Context            (Context, LogCxt (logC),
                                             readFromCache)
import           Twitter.Model              (TwitterError, UserTimeLine,
                                             apiError, createError, createTweet,
                                             credentialError)

cacheTimeLine :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m TwitterResponse
cacheTimeLine req = do
  cache <- ask
  liftIO $ logC cache ("Getting from cache " <> unpack (userName req))
  maybeToEither <$> liftIO (cache `readFromCache` userName req)
  where
    maybeToEither (Just val) = Just (Right val)
    maybeToEither Nothing    = Nothing

newHandle :: (MonadReader Context m, MonadIO m) => Handle m
newHandle = Handle { timeline = cacheTimeLine }
