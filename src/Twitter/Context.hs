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

import           Data.Cache     as C (Cache, insert, lookup)
import           Data.Text      (Text)
import           Twitter.Config (Config (environment), Environment)
import           Twitter.Model  (UserTimeLine)

data Context = Context
  { config :: Config
  , logger :: String -> IO ()
  , cache  :: C.Cache Text UserTimeLine
  }

class LogCxt a where
  logC :: a -> (String -> IO ())
instance LogCxt Context where
  logC = logger
instance LogCxt (String -> IO ()) where
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


buildCxt :: Config -> (String -> IO ()) -> C.Cache Text UserTimeLine -> Context
buildCxt = Context
