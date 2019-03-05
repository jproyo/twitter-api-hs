{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Twitter.Service
  (
    ApiTimeLine(..),
    CacheTimeLine(..),
    getUserTimeline,
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             ((>=>))
import           Control.Monad.Reader      (MonadReader, ReaderT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Twitter.Adapter           (TimeLineRequest, TimeLineResponse,
                                            TwitterResponse,
                                            createTimeLineRequest, timeline)
import           Twitter.CacheAdapter      as CA
import           Twitter.Context           (Context)
import qualified Twitter.TwitterAdapter    as TA



class (MonadReader Context m, Monad m) => CacheTimeLine m where
  fromCache  :: TimeLineRequest  -> m TwitterResponse
  storeCache :: TimeLineRequest -> TwitterResponse -> m TwitterResponse

class (MonadReader Context m, Monad m) => ApiTimeLine m where
  fromApi :: TimeLineRequest -> m TwitterResponse

instance ApiTimeLine (ReaderT Context IO) where
  fromApi = timeline TA.newHandle

instance CacheTimeLine (ReaderT Context IO) where
  fromCache  = timeline CA.newHandle
  storeCache = CA.cacheStoreTimeLine

getUserTimeline :: (ApiTimeLine m, CacheTimeLine m) =>
        Text -> Maybe Integer -> m TimeLineResponse
getUserTimeline userName limit = getTime req
  where req = createTimeLineRequest userName limit

getTime :: (ApiTimeLine m, CacheTimeLine m) => TimeLineRequest -> m TimeLineResponse
getTime req = do
  fromJust <$> runMaybeT (cache <|> api)
    where cache = MaybeT (fromCache req)
          api   = MaybeT ((fromApi >=> storeCache req) req)

