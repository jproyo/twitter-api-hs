{-# LANGUAGE RecordWildCards #-}

-- | This is an abstract interface for a simple Twitter service. It is intended to be
-- imported qualified as follows.
--
-- > import qualified Twitter.Service as Service
--
module Twitter.Adapter
  ( Handle(..)
  , TimeLineRequest( limit
                   , userName
                   )
  , TwitterHandle
  , TwitterResponse
  , createTimeLineRequest
  , timeline
  ) where

import Data.Text     ( Text )
import Twitter.Model ( TwitterError
                     , UserTimeLine
                     )

type HandleResponse e a = IO (Maybe (Either e a))

newtype Handle a e b = Handle { execute :: a -> HandleResponse e b }

type TwitterHandle = Handle TimeLineRequest TwitterError UserTimeLine

type TwitterResponse = HandleResponse TwitterError UserTimeLine

data TimeLineRequest = TimeLineRequest
  { userName :: Text
  , limit    :: Maybe Int
  }

createTimeLineRequest :: Text -> Maybe Int -> TimeLineRequest
createTimeLineRequest = TimeLineRequest

timeline :: TwitterHandle -> TimeLineRequest -> TwitterResponse
timeline = execute
