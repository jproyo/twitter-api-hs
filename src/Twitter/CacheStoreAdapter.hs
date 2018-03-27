{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Twitter.CacheStoreAdapter (
cacheStoreTimeLine
) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Reader.Class (ask)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import           Twitter.Adapter            (TimeLineRequest (..),
                                             TwitterResponse)
import           Twitter.Context            (Context, LogCxt (..), putInCache)

cacheStoreTimeLine :: (MonadReader Context m, MonadIO m) =>
        TimeLineRequest -> TwitterResponse -> m TwitterResponse
cacheStoreTimeLine req res = do
  cache <- ask
  liftIO $ either (\_ -> return ()) (putInCache cache (userName req)) (fromJust res)
  liftIO $ debug cache $
        "Store in Cache timeline for " <> userName req
  return res
