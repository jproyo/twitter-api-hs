module Twitter.Service
  (
  TwitterService(..),
  getUserTimeline
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Twitter.Adapter           (Handle, TimeLineRequest,
                                            TwitterHandle,
                                            createTimeLineRequest, timeline)
import           Twitter.CacheAdapter      as CA
import           Twitter.Context           (Context)
import           Twitter.Model             (TwitterError, UserTimeLine)
import qualified Twitter.TwitterAdapter    as TA

class Monad m => TwitterService m where
  getTimeLine :: Context -> TimeLineRequest -> m (Either TwitterError UserTimeLine)

instance TwitterService IO where
  getTimeLine cxt request = fromJust
    <$> runMaybeT (MaybeT (getFromCache cxt request)
               <|> MaybeT (getFromTwitter cxt request))

getFrom :: (Context -> IO TwitterHandle) -> Context -> TimeLineRequest -> IO (Maybe (Either TwitterError UserTimeLine))
getFrom handleBuilder cxt req = handleBuilder cxt >>= flip timeline req

getFromCache :: Context -> TimeLineRequest -> IO (Maybe (Either TwitterError UserTimeLine))
getFromCache = getFrom CA.newHandle

getFromTwitter :: Context -> TimeLineRequest -> IO (Maybe (Either TwitterError UserTimeLine))
getFromTwitter = getFrom TA.newHandle

getUserTimeline :: TwitterService m => Context -> Text -> Maybe Int -> m (Either TwitterError UserTimeLine)
getUserTimeline cxt userName limit = getTimeLine cxt (createTimeLineRequest userName limit)
