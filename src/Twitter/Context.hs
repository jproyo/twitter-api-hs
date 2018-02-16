{-# LANGUAGE FlexibleInstances #-}
module Twitter.Context
  (
  Context,
  LogCxt(..),
  ConfigCxt(..),
  CacheCxt(..),
  EnvCxt(..),
  buildCxt
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Cache             as C (Cache, insert, lookup, newCache)
import           Data.Text              (Text)
import           System.Clock           (fromNanoSecs)
import           System.Logger          as L (Level (..), Logger, debug,
                                              defSettings, err, info, new,
                                              setLogLevel, trace, warn)
import           System.Logger.Message  (Msg, msg)
import           Twitter.Config         (Config (..), Environment (..),
                                         getConfig)
import           Twitter.Model          (UserTimeLine)

data Context = Context
  { config :: Config
  , logger :: L.Logger
  , cache  :: C.Cache Text UserTimeLine
  }

class LogCxt a where
  logC :: a -> L.Logger
  putLog :: (MonadIO m) => (Logger -> (Msg -> Msg) -> m ()) -> a -> Text -> m ()
  putLog f c = f (logC c) . msg
  trace :: a -> Text -> IO ()
  trace = putLog L.trace
  debug :: a -> Text -> IO ()
  debug = putLog L.debug
  info :: a -> Text -> IO ()
  info = putLog L.info
  err :: a -> Text -> IO ()
  err = putLog L.err
  warn :: a -> Text -> IO ()
  warn = putLog L.warn
instance LogCxt Context where
  logC = logger
instance LogCxt L.Logger where
  logC = id

class ConfigCxt a where
  conf :: a -> Config
instance ConfigCxt Context where
  conf = config
instance ConfigCxt Config where
  conf = id

class EnvCxt a where
  env :: a -> Environment
instance EnvCxt Context where
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


getLevel :: Config -> Level
getLevel (Config _ Development) = Debug
getLevel (Config _ Production)  = Info
getLevel _                      = Trace

getCache :: IO (C.Cache Text UserTimeLine)
getCache = C.newCache (Just (fromNanoSecs 30000000000))

getLogger :: Config -> IO L.Logger
getLogger c = L.new (L.setLogLevel (getLevel c) L.defSettings)

buildCxt :: IO Context
buildCxt = getConfig >>= \c -> Context c <$> getLogger c <*> getCache
