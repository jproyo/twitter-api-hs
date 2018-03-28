{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Twitter.Service
  (
  GetUserTimeLineAdapter(..),
  TimeLineOpeartion(..),
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
import           Twitter.Context           (Context)
import qualified Twitter.TwitterAdapter    as TA


data TimeLineOpeartion m =
          GetUserTimeLine { get :: TimeLineRequest -> m TwitterResponse }
        | StoreUserTimeLine { store ::
                TimeLineRequest -> Maybe TimeLineResponse -> m TwitterResponse }

data UserTimeLineOp = GetFromCache | GetFromAPI | StoreCache

class (MonadReader Context m, Monad m) => GetUserTimeLineAdapter a m where
        execute :: a -> TimeLineOpeartion m

instance (MonadReader Context m, MonadIO m) =>
        GetUserTimeLineAdapter UserTimeLineOp m where
        execute GetFromCache = GetUserTimeLine (timeline CA.newHandle)
        execute GetFromAPI   = GetUserTimeLine (timeline TA.newHandle)
        execute StoreCache   = StoreUserTimeLine CA.cacheStoreTimeLine

getUserTimeline :: (MonadReader Context m, MonadIO m) =>
        Text -> Maybe Int -> m TimeLineResponse
getUserTimeline userName limit = getTimeLine req GetFromCache GetFromAPI StoreCache
        where req = createTimeLineRequest userName limit


getTimeLine :: (MonadReader Context m, GetUserTimeLineAdapter a m) =>
        TimeLineRequest -> a -> a -> a -> m TimeLineResponse
getTimeLine req getFromCache getFromApi storeIntoCache =
                fromJust <$> runMaybeT (cache <|> apiAndStore)
        where cache       = MaybeT (fromCache req)
              apiAndStore = MaybeT ((api >=> storeCache) req)
              fromCache   = get (execute getFromCache)
              api         = get (execute getFromApi)
              storeCache  = store (execute storeIntoCache) req
