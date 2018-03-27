{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Twitter.Adapter
  (
    Handle(..)
  , TimeLineRequest(userName,limit)
  , TwitterResponse
  , TimeLineResponse
  , createTimeLineRequest
  ) where

import           Control.Monad.Reader   (MonadReader)
import           Data.Text              (Text)
import           Twitter.Context        (Context)
import           Twitter.Model          (TwitterError, UserTimeLine)

newtype Handle m = Handle
  { timeline :: (MonadReader Context m, Monad m) =>
        TimeLineRequest -> m TwitterResponse }

type TwitterResponse = Maybe (Either TwitterError UserTimeLine)
type TimeLineResponse = Either TwitterError UserTimeLine

data TimeLineRequest = TimeLineRequest
  { userName :: Text
  , limit    :: Maybe Int
  } deriving (Show)

createTimeLineRequest :: Text -> Maybe Int -> TimeLineRequest
createTimeLineRequest = TimeLineRequest
