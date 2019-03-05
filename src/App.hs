{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module App (runApp, app) where

import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       asks, lift, runReaderT)
import           Control.Monad.Reader.Class           (ask)
import           Data.Aeson                           (Value (..), object, (.=))
import           Data.ByteString.Char8                (pack)
import           Data.Default                         (def)
import           Data.Text.Lazy                       (Text)
import           Network.HTTP.Types.Status            (internalServerError500,
                                                       mkStatus, notFound404)
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       setFdCacheDuration,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Twitter.Config                       (Environment (..))
import           Twitter.Context                      (Context, EnvCtx (..),
                                                       buildCtx)
import           Twitter.Model                        (TwitterError (..))
import           Twitter.Service                      (getUserTimeline,
                                                       runService)
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

runConfig :: Context -> ContextM a -> IO a
runConfig cxt m = runReaderT (runContextM m) cxt

app :: IO Application
app = do
  ctx <- buildCtx
  scottyAppT (runConfig ctx) (application (env ctx))

runApp :: IO ()
runApp = buildCtx >>= runApplication

runApplication :: Context -> IO ()
runApplication ctx = scottyOptsT (getOptions e) (runConfig ctx) (application e)
  where
    e = env ctx

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
  ctx      <- lift ask
  userName <- param "userName"
  limit    <- param "limit" `rescue` (\_ -> return 10)
  timeline <- liftIO $ runReaderT (runService $ getUserTimeline userName (Just limit)) ctx
  let statusAndResponse err = status (mkStatus (fromIntegral $ code err) (pack $ show err)) >> json err
      in either statusAndResponse json timeline

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null
