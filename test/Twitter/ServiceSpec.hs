{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Twitter.ServiceSpec (spec) where

import           Control.Monad.Reader
import           Data.Text
import           Test.Hspec
import           Twitter.Adapter
import           Twitter.Context
import           Twitter.Model
import           Twitter.Service


spec :: Spec
spec = describe "getTimeLine using mocking" $ do
        it "should responds from cache if exist there first" $ do
                cache <- getFrom MockCache GetDoNothing StoreDoNothing
                cache `shouldBe` expected "cache" "user"
        it "should responds from twitter if it doesnt exist in cache" $ do
                twitter <- getFrom GetDoNothing MockApi MockStore
                twitter `shouldBe` expected "twitter" "user"
        it "should responds from cache after first access" $ do
                _ <- getFrom GetDoNothing MockApi StoreCache
                twitterCache <- getFrom GetFromCache MockApi MockStore
                twitterCache `shouldBe` expected "twitter" "user"


data MockOperation = MockCache | MockApi | MockStore | StoreDoNothing | GetDoNothing

instance (MonadReader Context m, MonadIO m) =>
        GetUserTimeLineAdapter MockOperation m where
        execute MockCache = GetUserTimeLine (\_ -> return $ return $ expected "cache" "user")
        execute MockApi   = GetUserTimeLine (\_ -> return $ return $ expected "twitter" "user")
        execute MockStore = StoreUserTimeLine (\_ _ -> return $ return $ expected "twitter" "user")
        execute StoreDoNothing = StoreUserTimeLine (\_ _ -> return Nothing)
        execute GetDoNothing = GetUserTimeLine (\_ -> return Nothing)


getFrom :: (GetUserTimeLineAdapter a (ReaderT Context IO)
          , GetUserTimeLineAdapter b (ReaderT Context IO)
          , GetUserTimeLineAdapter c (ReaderT Context IO)
          , MonadIO m) =>
        a -> b -> c -> m TimeLineResponse
getFrom mockCache mockTwitter mockStore = do
        ctx <- liftIO buildCtx
        liftIO $ runReaderT (getTimeLine request mockCache mockTwitter mockStore) ctx


request :: TimeLineRequest
request = createTimeLineRequest "someuser" (Just 10)

expected :: Text -> Text -> TimeLineResponse
expected textTweet user = Right
        [ createTweet textTweet user Nothing 10 4 ]
