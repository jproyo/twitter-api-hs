{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Twitter.Service
  (
    ApiTimeLine(..),
    CacheTimeLine(..),
    unServiceApp,
    getUserTimeline,
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             ((>=>))
import           Control.Monad.Reader      (MonadIO, MonadReader, ReaderT, asks,
                                            liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Cache                as C (insert, lookup)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           System.Logger             as L (Level (Debug, Error), Logger,
                                                 Msg, ToBytes, debug, err, info,
                                                 msg)
import           Twitter.Adapter           (TimeLineRequest, TimeLineResponse,
                                            TwitterResponse,
                                            createTimeLineRequest, timeline)
import           Twitter.CacheAdapter      as CA
import           Twitter.Context           (Cache (..), Context (cache, logger),
                                            Logging (..))
import qualified Twitter.TwitterAdapter    as TA


newtype ServiceApp a = ServiceApp { unServiceApp :: ReaderT Context IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context)

instance Logging ServiceApp where
  logMsg Debug = logWith L.debug
  logMsg Error = logWith L.err
  logMsg _     = logWith L.info

logWith
  :: (MonadReader Context m, ToBytes a)
  => (Logger -> (Msg -> Msg) -> m b)
  -> a
  -> m b
logWith f t = asks logger >>= flip f (msg t)

instance Cache ServiceApp where
  put k v = asks cache >>= \ch -> liftIO $ C.insert ch k v >> return ()
  get k = asks cache >>= \ch -> liftIO $ C.lookup ch k >>= return


class MonadReader Context m => CacheTimeLine m where
  fromCache  :: TimeLineRequest  -> m TwitterResponse
  storeCache :: TimeLineRequest -> TwitterResponse -> m TwitterResponse

class MonadReader Context m => ApiTimeLine m where
  fromApi :: TimeLineRequest -> m TwitterResponse

instance ApiTimeLine ServiceApp where
  fromApi = timeline TA.newHandle

instance CacheTimeLine ServiceApp where
  fromCache  = timeline CA.newHandle
  storeCache = CA.cacheStoreTimeLine

getUserTimeline :: (ApiTimeLine m, CacheTimeLine m) =>
        Text -> Maybe Integer -> m TimeLineResponse
getUserTimeline userName limit = getTime req
  where req = createTimeLineRequest userName limit

getTime
  :: (ApiTimeLine m, CacheTimeLine m)
  => TimeLineRequest
  -> m TimeLineResponse
getTime req =
  fromJust <$> runMaybeT (cacheResult <|> apiResult)
    where cacheResult = MaybeT (fromCache req)
          apiResult   = MaybeT ((fromApi >=> storeCache req) req)

