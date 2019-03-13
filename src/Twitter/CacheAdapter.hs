{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Twitter.CacheAdapter (
  newHandle
, cacheStoreTimeLine
) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Reader.Class (ask)
import           Core.Utils                 (maybeToEither)
import           Data.Maybe                 (fromJust, maybe)
import           Data.Monoid                ((<>))
import           Twitter.Adapter            (Handle (..), TimeLineRequest (..),
                                             TwitterResponse)
import           Twitter.Context            (Context, Logging (debug),
                                             putInCache, readFromCache)

cacheStoreTimeLine :: (MonadReader Context m, MonadIO m, Logging m) =>
    TimeLineRequest -> TwitterResponse -> m TwitterResponse
cacheStoreTimeLine req res = do
    cache <- ask
    liftIO $ either
            (\_ -> return ())
            (putInCache cache (userName req))
            (fromJust res)
    debug $ "Store in Cache timeline for " <> userName req
    return res

cacheTimeLine :: (MonadReader Context m, MonadIO m, Logging m) =>
    TimeLineRequest -> m TwitterResponse
cacheTimeLine req = do
    cache <- ask
    fromCache <- liftIO (cache `readFromCache` userName req)
    debug $ "Cache timeline for " <> userName req <>
        " value " <> maybe "[Nothing]" (const "[Cached Timeline]") fromCache
    return $ maybeToEither fromCache

newHandle :: (MonadIO m, Logging m) => Handle m
newHandle = Handle { timeline = cacheTimeLine }
