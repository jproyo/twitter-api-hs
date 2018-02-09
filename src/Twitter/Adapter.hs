{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Twitter.Adapter
  (
    Handle(..)
  , TimeLineRequest(userName,limit)
  , TwitterResponse
  , createTimeLineRequest
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader)
import           Data.Text              (Text)
import           Twitter.Context        (Context)
import           Twitter.Model          (TwitterError, UserTimeLine)

newtype Handle m = Handle
  { timeline :: (MonadReader Context m, MonadIO m) => TimeLineRequest -> m TwitterResponse }

type TwitterResponse = Maybe (Either TwitterError UserTimeLine)

data TimeLineRequest = TimeLineRequest
  { userName :: Text
  , limit    :: Maybe Int
  }

createTimeLineRequest :: Text -> Maybe Int -> TimeLineRequest
createTimeLineRequest = TimeLineRequest
