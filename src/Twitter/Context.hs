{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Twitter.Context
  (
  Context(..),
  Logging(..),
  ConfigCxt(..),
  CacheCxt(..),
  EnvCtx(..),
  buildCtx
  ) where

import           Control.Monad.Reader (MonadReader)
import           Data.Cache           as C (Cache, insert, lookup, newCache)
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

class CacheCxt a where
    putInCache :: a -> Text -> UserTimeLine -> IO ()
    readFromCache :: a -> Text -> IO (Maybe UserTimeLine)
instance CacheCxt (C.Cache Text UserTimeLine) where
    putInCache     = C.insert
    readFromCache  = C.lookup
instance CacheCxt Context where
    putInCache    cxt  = putInCache    (cache cxt)
    readFromCache cxt  = readFromCache (cache cxt)

class MonadReader Context m => Logging m where
  {-# MINIMAL logMsg  #-}
  logMsg :: Level -> Text -> m ()

  info :: Text -> m ()
  info = logMsg Info

  debug :: Text -> m ()
  debug = logMsg Debug

  err :: Text -> m ()
  err = logMsg Error


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
