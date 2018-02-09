{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module App (runApp, app) where

import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       asks, lift, runReaderT)
import           Control.Monad.Reader.Class           (ask)
import           Control.Monad.Trans.Class            (MonadTrans)
import           Data.Aeson                           (Value (..), object, (.=))
import           Data.ByteString.Char8                (pack)
import           Data.Cache                           as C (Cache, newCache)
import           Data.Default                         (def)
import           Data.Text.Lazy                       (Text)
import           Network.HTTP.Types.Status            (created201,
                                                       internalServerError500,
                                                       mkStatus, notFound404)
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       setFdCacheDuration,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Clock                         (fromNanoSecs)
import           Twitter.Config                       (Config (..),
                                                       Environment (..),
                                                       getConfig, twitterEncKey)
import           Twitter.Context                      (ConfigCxt (..), Context,
                                                       EnvCxt (..), buildCxt)
import           Twitter.Model                        (TwitterError (..),
                                                       UserTimeLine)
import           Twitter.Service                      (getUserTimeline)
import           Web.Scotty.Trans                     (ActionT, Options,
                                                       ScottyT, defaultHandler,
                                                       get, json, middleware,
                                                       notFound, param, rescue,
                                                       scottyAppT, scottyOptsT,
                                                       settings, showError,
                                                       status, verbose)



newtype ContextM a = ContextM
 { runContextM :: ReaderT Context IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context)

type Error = Text
type Action = ActionT Error ContextM ()

getSettings :: Environment -> Settings
getSettings e = setPort 8080 $ case e of
  Development -> setFdCacheDuration 0 s
  Production  -> s
  Test        -> s
  where
    s = defaultSettings

getOptions :: Environment -> Options
getOptions e = def
  { settings = getSettings e
  , verbose = case e of
    Development -> 1
    Production  -> 0
    Test        -> 0
  }

defaultH :: Error -> Action
defaultH x = do
  e <- lift $ asks env
  status internalServerError500
  json $ case e of
    Development -> object [ "error" .= showError x ]
    Production  -> Null
    Test        -> object [ "error" .= showError x ]

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production  = logStdout
loggingM _           = id

buildContext :: IO Context
buildContext = do
  config <- getConfig
  cache <- C.newCache (Just (fromNanoSecs 30000000000))
  return $ buildCxt config putStrLn cache

runConfig :: Context -> ContextM a -> IO a
runConfig cxt m = runReaderT (runContextM m) cxt

app :: IO Application
app = do
  cxt <- buildContext
  scottyAppT (runConfig cxt) (application (env cxt))

runApp :: IO ()
runApp = buildContext >>= runApplication

runApplication :: Context -> IO ()
runApplication cxt = scottyOptsT (getOptions e) (runConfig cxt) (application e)
  where
    e = env cxt

application :: Environment -> ScottyT Error ContextM ()
application e = do
  middleware (loggingM e)
  defaultHandler defaultH
  get "/" rootAction
  get "/user/:userName/timeline" userTimelineAction
  notFound notFoundA

rootAction :: Action
rootAction = json $ object
  [ "endpoints" .= object
    [ "user_timeline" .= String "/user/{userName}/timeline"
    ]
  ]

userTimelineAction :: Action
userTimelineAction = do
  cxt      <- lift ask
  userName <- param "userName"
  limit    <- param "limit" `rescue` (\x -> return 10)
  timeline <- liftIO $ runReaderT (getUserTimeline userName (Just limit)) cxt
  let statusAndResponse err = status (mkStatus (code err) (pack $ show err)) >> json err
      in either statusAndResponse json timeline

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null
