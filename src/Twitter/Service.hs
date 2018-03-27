{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Twitter.Service
  (
  GetUserTimeLine,
  StoreUserTimeLine,
  getUserTimeline,
  getTimeLine
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             ((>=>))
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Twitter.Adapter           (TimeLineRequest, TimeLineResponse,
                                            TwitterResponse,
                                            createTimeLineRequest, timeline)
import           Twitter.CacheAdapter      as CA
import           Twitter.CacheStoreAdapter as CS
import           Twitter.Context           (Context)
import qualified Twitter.TwitterAdapter    as TA

type GetUserTimeLine m = TimeLineRequest -> m TwitterResponse
type StoreUserTimeLine m = TimeLineRequest -> Maybe TimeLineResponse -> m TwitterResponse

storeInCache :: (MonadReader Context m, MonadIO m) => StoreUserTimeLine m
storeInCache = CS.cacheStoreTimeLine

getFromCache :: (MonadReader Context m, MonadIO m) => GetUserTimeLine m
getFromCache = timeline CA.newHandle

getFromTwitter :: (MonadReader Context m, MonadIO m) => GetUserTimeLine m
getFromTwitter = timeline TA.newHandle


getUserTimeline :: (MonadReader Context m, MonadIO m) =>
        Text -> Maybe Int -> m TimeLineResponse
getUserTimeline userName limit = getTimeLine req cache api store
        where req   = createTimeLineRequest userName limit
              cache = getFromCache
              api   = getFromTwitter
              store = storeInCache

getTimeLine :: MonadReader Context m => TimeLineRequest ->
        GetUserTimeLine m -> GetUserTimeLine m ->
        StoreUserTimeLine m -> m TimeLineResponse
getTimeLine request fromCache fromAPI storeCache =
        fromJust <$> runMaybeT (cache <|> apiAndStore)
        where cache       = MaybeT (fromCache request)
              store       = storeCache request
              apiAndStore = MaybeT ((fromAPI >=> store) request)
