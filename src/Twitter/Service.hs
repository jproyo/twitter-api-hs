{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

getTimeLine :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m (Either TwitterError UserTimeLine)
getTimeLine request = fromJust
  <$> runMaybeT (MaybeT (getFromCache   request)
             <|> MaybeT (getFromTwitter request))

getFromCache :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m (Maybe (Either TwitterError UserTimeLine))
getFromCache = timeline CA.newHandle

getFromTwitter :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m (Maybe (Either TwitterError UserTimeLine))
getFromTwitter = timeline TA.newHandle

getUserTimeline :: (MonadReader Context m, MonadIO m) => Text -> Maybe Int -> m (Either TwitterError UserTimeLine)
getUserTimeline userName limit = getTimeLine $ createTimeLineRequest userName limit
