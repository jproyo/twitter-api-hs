{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Twitter.Service
  (
  getUserTimeline
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Twitter.Adapter           (TimeLineRequest,
                                            createTimeLineRequest, timeline)
import           Twitter.CacheAdapter      as CA
import           Twitter.Context           (Context)
import           Twitter.Model             (TwitterError, UserTimeLine)
import qualified Twitter.TwitterAdapter    as TA

type TimeLineResponse = Either TwitterError UserTimeLine
type GetUserTimeLine m = TimeLineRequest -> m (Maybe TimeLineResponse)

getFromCache :: (MonadReader Context m, MonadIO m) =>
        TimeLineRequest -> m (Maybe TimeLineResponse)
getFromCache = timeline CA.newHandle

getFromTwitter :: (MonadReader Context m, MonadIO m) =>
        TimeLineRequest -> m (Maybe TimeLineResponse)
getFromTwitter = timeline TA.newHandle

getTimeLine :: MonadReader Context m => TimeLineRequest ->
        GetUserTimeLine m -> GetUserTimeLine m -> m TimeLineResponse
getTimeLine request fromCache fromAPI = fromJust <$>
        runMaybeT (MaybeT (fromCache request)
               <|> MaybeT (fromAPI   request))

getUserTimeline :: (MonadReader Context m, MonadIO m) =>
        Text -> Maybe Int -> m TimeLineResponse
getUserTimeline userName limit = getTimeLine
        (createTimeLineRequest userName limit) getFromCache getFromTwitter
