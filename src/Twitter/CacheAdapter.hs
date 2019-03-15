{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Twitter.CacheAdapter (
  newHandle
, cacheStoreTimeLine
) where

import           Core.Utils      (maybeToEither)
import           Data.Maybe      (fromJust, maybe)
import           Data.Monoid     ((<>))
import           Twitter.Adapter (Handle (..), TimeLineRequest (..),
                                  TwitterResponse)
import           Twitter.Context (Cache (..), Logging (debug))


cacheStoreTimeLine
  :: (Logging m, Cache m)
  => TimeLineRequest
  -> TwitterResponse
  -> m TwitterResponse
cacheStoreTimeLine req res = do
    either (\_ -> return ()) (put $ userName req) (fromJust res)
    debug $ "Store in Cache timeline for " <> userName req
    return res

cacheTimeLine
  :: (Logging m, Cache m)
  => TimeLineRequest
  -> m TwitterResponse
cacheTimeLine req = do
    fromCache <- get (userName req)
    debug
      $ "Cache timeline for "
      <> userName req
      <> " value "
      <> maybe "[Nothing]" (const "[Cached Timeline]") fromCache
    return $ maybeToEither fromCache

newHandle :: (Logging m, Cache m) => Handle m
newHandle = Handle { timeline = cacheTimeLine }
