{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Twitter.Context
  (
  Context(..),
  Logging(..),
  Twitter.Context.Cache(..),
  ConfigCxt(..),
  EnvCtx(..),
  buildCtx
  ) where

import           Control.Monad.Reader (MonadReader)
import           Data.Cache           as C (newCache, Cache)
import           Data.Text            (Text)
import           System.Clock         (fromNanoSecs)
import           System.Logger        as L (Level (..), Logger, defSettings,
                                            new, setLogLevel)
import           Twitter.Config       (Config (..), Environment (..), getConfig)
import           Twitter.Model        (UserTimeLine)

data Context = Context
  { config :: Config
  , logger :: L.Logger
  , cache  :: C.Cache Text UserTimeLine
  }

class ConfigCxt a where
    conf :: a -> Config
instance ConfigCxt Context where
    conf = config
instance ConfigCxt Config where
    conf = id

class EnvCtx a where
    env :: a -> Environment
instance EnvCtx Context where
    env = environment . config

class MonadReader Context m => Logging m where
  {-# MINIMAL logMsg  #-}
  logMsg :: Level -> Text -> m ()

  info :: Text -> m ()
  info = logMsg Info

  debug :: Text -> m ()
  debug = logMsg Debug

  err :: Text -> m ()
  err = logMsg Error

class (MonadReader Context m) => Cache m where
  {-# MINIMAL put, get #-}
  put :: Text -> UserTimeLine -> m ()
  get :: Text -> m (Maybe UserTimeLine)

getLevel :: Config -> Level
getLevel (Config _ Development) = Debug
getLevel (Config _ Production)  = Info
getLevel _                      = Trace

getCache :: IO (C.Cache Text UserTimeLine)
getCache = C.newCache (Just (fromNanoSecs 30000000000))

getLogger :: Config -> IO L.Logger
getLogger c = L.new (L.setLogLevel (getLevel c) L.defSettings)

buildCtx :: IO Context
buildCtx = getConfig >>= \c -> Context c <$> getLogger c <*> getCache
