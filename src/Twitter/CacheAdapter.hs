{-# LANGUAGE FlexibleContexts #-}
module Twitter.CacheAdapter (
newHandle
) where

import           Control.Concurrent.MVar    (newMVar, withMVar)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, lift, runReaderT)
import           Control.Monad.Reader.Class (ask)
import           Data.Text                  (Text)
import           Twitter.Adapter            (Handle (..), TimeLineRequest (..),
                                             TwitterHandle, TwitterResponse,
                                             execute)
import           Twitter.Context            (Context, readFromCache)
import           Twitter.Model              (TwitterError, UserTimeLine,
                                             apiError, createError, createTweet,
                                             credentialError)

cacheTimeLine :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m (Maybe (Either TwitterError UserTimeLine))
cacheTimeLine req = do
  cache <- ask
  maybeToEither <$> liftIO (readFromCache cache (userName req))
  where
    maybeToEither (Just val) = Just (Right val)
    maybeToEither Nothing    = Nothing

newHandle :: Context -> IO TwitterHandle
newHandle cxt = do
  mutex <- newMVar ()
  return Handle
    { execute = \timelineReq ->
          withMVar mutex $ \() -> runReaderT (cacheTimeLine timelineReq) cxt
    }
