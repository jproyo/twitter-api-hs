{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Twitter.CacheAdapter (
newHandle
) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Reader.Class (ask)
import           Data.Maybe                 (maybe)
import           Data.Monoid                ((<>))
import           Twitter.Adapter            (Handle (..), TimeLineRequest (..),
                                             TwitterResponse)
import           Twitter.Context            (Context, LogCxt (..),
                                             readFromCache)

cacheTimeLine :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m TwitterResponse
cacheTimeLine req = do
  cache <- ask
  fromCache <- liftIO (cache `readFromCache` userName req)
  liftIO $ debug cache $ "Cache timeline for " <> userName req <> " value " <> maybe "[Nothing]" (\_ -> "[Cached Timeline]") fromCache
  return $ maybeToEither fromCache
  where
    maybeToEither (Just val) = Just (Right val)
    maybeToEither Nothing    = Nothing

newHandle :: (MonadReader Context m, MonadIO m) => Handle m
newHandle = Handle { timeline = cacheTimeLine }
