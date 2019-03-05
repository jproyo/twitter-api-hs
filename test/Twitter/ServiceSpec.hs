{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Twitter.ServiceSpec (spec) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Data.Text
import Test.Hspec
import Twitter.Adapter
import Twitter.Context
import Twitter.Model
import Twitter.Service


spec :: Spec
spec = describe "getTimeLine using mocking" $ do
    it "should responds from cache if exist there first" $ do
        ctx   <- liftIO buildCtx
        cache <- runIdentityT $ runReaderT (unwrap (getUserTimeline @InCache "someuser" (Just 10))) ctx
        cache `shouldBe` expected "cache" "user"
    it "should responds from twitter if it doesnt exist in cache" $ do
        ctx     <- liftIO buildCtx
        twitter <- runIdentityT $ runReaderT (unwrapNo (getUserTimeline @NoCache "someuser" (Just 10))) ctx
        twitter `shouldBe` expected "twitter" "user"

newtype InCache a = InCache { unwrap :: ReaderT Context (IdentityT IO) a }
                 deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context)

newtype NoCache a = NoCache { unwrapNo :: ReaderT Context (IdentityT IO) a }
                 deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context)

instance CacheTimeLine InCache where
  fromCache _ = return $ Just (expected "cache" "user")
  storeCache _ _ = return Nothing

instance ApiTimeLine InCache where
  fromApi _ = error "Never called"

instance CacheTimeLine NoCache where
  fromCache _ = return Nothing
  storeCache _ _ = return $ Just (expected "twitter" "user")

instance ApiTimeLine NoCache where
  fromApi _ = return $ Just (expected "twitter" "user")

expected :: Text -> Text -> TimeLineResponse
expected textTweet user = Right
        [ createTweet textTweet user Nothing 10 4 ]
